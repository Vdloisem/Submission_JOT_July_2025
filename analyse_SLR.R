# R Script for SLR Data Analysis (LOESS Only + Full Year Axis)
# Script Name: analyze_SLR.R
# Date: 2025-07-26

# --- Preparation: Install and Load Libraries ---
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# --- Step 1: Load the Data ---

file_path <- "SLR_Towards_a_Unified_Framework_for_Programming_Paradigms.csv"
slr_data <- read_csv(file_path)

print("Data loaded successfully.")

# --- Step 2: Analysis #1 - LOESS Trendline (from 1980) ---

# Filter data from 1980 onward
slr_data_filtered <- slr_data %>% filter(Year >= 1980)

# Count number of publications per year and RQ
yearly_counts <- slr_data_filtered %>%
  count(Year, primary_rq)

# Fill missing year/RQ combinations with 0
all_years <- 1980:2022
all_rqs <- unique(yearly_counts$primary_rq)

complete_data <- expand_grid(
  Year = all_years,
  primary_rq = all_rqs
) %>%
  left_join(yearly_counts, by = c("Year", "primary_rq")) %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(primary_rq, Year)

# Plot LOESS-only curve with full-year axis
trend_plot_loess <- ggplot(complete_data, aes(x = Year, y = n, color = primary_rq)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.5, linewidth = 1.2) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Smoothed Trend of Publications by Research Question",
    subtitle = "Smoothed trend using LOESS interpolation (1980–2022)",
    x = "Publication Year",
    y = "Estimated Publications per Year (LOESS)",
    color = "Research Question"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_continuous(
    breaks = seq(1980, 2022, by = 2),
    limits = c(1980, 2022)
  )

# Save the plot
ggsave("figure_temporal_trend_loess_only.pdf", plot = trend_plot_loess, width = 8, height = 5)
print("LOESS-only trend plot saved as 'figure_temporal_trend_loess_only.pdf'")

# --- Step 3: Bar Charts by RQ ---

# --- RQ1 ---

bar_chart_rq1 <- slr_data %>%
  filter(primary_rq == "RQ1") %>%
  count(venue_type, name = "count") %>%
  mutate(venue_type = reorder(venue_type, count)) %>%
  ggplot(aes(x = venue_type, y = count, fill = venue_type)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = count), hjust = -0.3) +
  labs(
    title = "Venue Type Distribution for RQ1 (Classification)",
    x = "Venue Type",
    y = "Number of Primary Studies"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(bar_chart_rq1)

ggsave("bar_chart_venues_rq1.pdf", plot = bar_chart_rq1, width = 8, height = 6)
print("RQ1 venue distribution bar chart saved as 'bar_chart_venues_rq1.pdf'")

# --- RQ2 ---

bar_chart_rq2 <- slr_data %>%
  filter(primary_rq == "RQ2") %>%
  count(venue_type, name = "count") %>%
  mutate(venue_type = reorder(venue_type, count)) %>%
  ggplot(aes(x = venue_type, y = count, fill = venue_type)) +
  geom_col() +
  coord_flip() +
  geom_text(aes(label = count), hjust = -0.3) +
  labs(
    title = "Venue Type Distribution for RQ2 (Reconstruction)",
    x = "Venue Type",
    y = "Number of Primary Studies"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(bar_chart_rq2)

ggsave("bar_chart_venues_rq2.pdf", plot = bar_chart_rq2, width = 8, height = 6)
print("RQ2 venue distribution bar chart saved as 'bar_chart_venues_rq2.pdf'")


slr_raw <- read_csv("SLR_Towards_a_Unified_Framework_for_Programming_Paradigms.csv")

slr_granular <- slr_clean %>%
  filter(Year >= 2000) %>% 
  mutate(Period = case_when(
    Year <= 2009 ~ "1. Foundational Era (2000-2009)",
    Year >= 2010 & Year <= 2019 ~ "2. Classification Surge (2010-2019)",
    Year >= 2020 ~ "3. Reconstructive Turn (2020-2025)",
    TRUE ~ "Other"
  )) %>%
  filter(Period != "Other")

granular_stats <- slr_granular %>%
  group_by(Period, primary_rq) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Period, primary_rq, fill = list(Count = 0)) %>%
  group_by(Period) %>%
  mutate(Total = sum(Count)) %>%
  mutate(Percentage = round(Count / Total * 100, 1))

print("--- CONVERGENCE ANALYSIS ---")
print(granular_stats)

plot_granular <- ggplot(granular_stats, aes(x = Period, y = Percentage, fill = primary_rq)) +
  geom_col(position = "dodge", width = 0.7) +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_dodge(width = 0.7), vjust = -0.5, size = 4) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Evidence of the 'Reconstructive Turn' (2020s)",
       subtitle = "Percentage of studies per Research Question over time",
       y = "Share of Publications (%)",
       fill = "Research Focus") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))

print(plot_granular)
ggsave("figure_granular_trend.pdf", plot = plot_granular, width = 8, height = 5)