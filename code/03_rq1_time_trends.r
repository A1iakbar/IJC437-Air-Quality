# ============================================================
# IJC437 - Introduction to Data Science (Individual Coursework)
# Script: 03_rq1_time_trends.R
#
# Purpose:
# - Address RQ1 by analysing how PM2.5 in London varies over time:
#   (i) seasonal patterns (month-level variation) and
#   (ii) long-term trends (multi-year changes).
#
# Research Questions supported:
# - RQ1: How do PM2.5 concentrations in London vary over time in terms of
#        seasonal patterns and long-term trends?
#
# Inputs:
# - data/processed/merged_pm25_weather.csv
#
#
# Reproducibility notes:
# - This script saves figures/tables to disk (minimal console printing).
# - Paths follow a consistent repo structure (data/processed, output/figures, output/tables).
#
# ============================================================



# ==================================================
# Installing and Importing Necessary Libraries
# ==================================================

library(tidyverse)
library(lubridate)
library(readr)
library(ggplot2)
library(scales)
library(zoo)
library(dplyr)
library(forecast)
library(tidyr)



# ==================================================
# Loading Merged Data and Basic Setup
# ==================================================
out_dir_plot <- "output/figures"
out_dir_table <- "output/tables"
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)


df <- read_csv("data/processed/merged_pm25_weather.csv", show_col_types = FALSE)
summary(df)
df <- df %>%
  mutate(date = as.Date(date))

df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    mean_pm25 = mean(pm25_london_mean, na.rm = TRUE),
    median_pm25 = median(pm25_london_mean, na.rm = TRUE),
    days = n(),
    .groups = "drop"
  )



# ==================================================
# Visualizations, Tables and Statistical Tests
# ==================================================

# Daily Mean PM2.5 Concentration in London
p_ts <- ggplot(df, aes(x = date, y = pm25_london_mean)) +
  geom_line(
    color = "#dd9c47",
    linewidth = 0.6,
    alpha = 0.9
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  labs(
    title = "Daily Mean PM2.5 Concentration in London",
    subtitle = "City-wide average across monitoring stations",
    x = NULL,
    y = expression(PM[2.5]~(mu*g/m^3)),
    caption = "Source: OpenAQ"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text = element_text(color = "grey20"),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(10, 20, 10, 10),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.2
    ),
  )

ggsave(
  filename = file.path(out_dir_plot, "Fig1_daily_pm25_timeseries.png"),
  plot = p_ts,
  width = 11,
  height = 4.5,
  dpi = 300
)


# Monthly Distribution of PM2.5 Concentrations in London
p_month_box <- df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  ggplot(aes(x = month, y = pm25_london_mean)) +
  geom_boxplot(
    fill = "#ddad34",
    color = "black",
    linewidth = 0.6,
    outlier.alpha = 0.35,
    outlier.size = 1.2
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Monthly Distribution of PM2.5 Concentrations in London",
    subtitle = "Boxplots based on daily city-wide mean values",
    x = NULL,
    y = expression(PM[2.5]~(mu*g/m^3)),
    caption = "Boxes show median and interquartile range; points indicate outliers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text = element_text(color = "grey20"),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),

    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.1
    ),

    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(12, 12, 12, 12)
  )

ggsave(
  filename = file.path(out_dir_plot, "Fig2_seasonality_monthly_boxplot.png"),
  plot = p_month_box,
  width = 8.5,
  height = 4.8,
  dpi = 300
)

# Statistical Analysis: Seasonality Kruskal–Wallis test (Month vs PM2.5)

df_rq1_kw <- df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  select(pm25_london_mean, month) %>%
  filter(!is.na(pm25_london_mean))


kw_month <- kruskal.test(pm25_london_mean ~ month, data = df_rq1_kw)
kw_month

# =========================================================
# Output:
#
#        Kruskal-Wallis rank sum test
#
# data:  pm25_london_mean by month
# Kruskal-Wallis chi-squared = 274.69, df = 11, p-value < 2.2e-16
# 
# =========================================================



# Distribution of Daily PM2.5 Concentrations in London - Histogram
mu <- mean(df$pm25_london_mean, na.rm = TRUE)
sigma <- sd(df$pm25_london_mean, na.rm = TRUE)

p_hist <- ggplot(df, aes(x = pm25_london_mean)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "#d1b423",
    color = "black",
    linewidth = 0.3
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = mu, sd = sigma),
    color = "#c9710b",
    linewidth = 1
  ) +
  labs(
    title = "Distribution of Daily PM2.5 Concentrations in London",
    subtitle = "Histogram with fitted normal distribution",
    x = expression(PM[2.5]~(mu*g/m^3)),
    y = "Density",
    caption = "Red curve shows normal distribution with empirical mean and standard deviation"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),

    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.1
    ),

    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(12, 12, 12, 12)
  )

ggsave(
  filename = file.path(out_dir_plot, "Fig3_pm25_histogram_normal.png"),
  plot = p_hist,
  width = 8,
  height = 4.8,
  dpi = 300
)



# Smoothed trend in PM2.5 concentrations
p_loess <- ggplot(df, aes(date, pm25_london_mean)) +
  geom_point(alpha = 0.05) +
  geom_smooth(method = "loess", span = 0.2, color = "red") +
  labs(
    title = "Smoothed trend in PM2.5 concentrations",
    x = "Date",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  theme_minimal()

ggsave(
  file.path(out_dir_plot, "Fig4_loess_trend.png"),
  p_loess, width = 8, height = 4, dpi = 300
)


# Yearly Mean and Median PM2.5 Concentration in London
yearly_pm25 <- df %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  summarise(
    mean_pm25   = mean(pm25_london_mean, na.rm = TRUE),
    median_pm25 = median(pm25_london_mean, na.rm = TRUE),
    .groups = "drop"
  )

yearly_long <- yearly_pm25 %>%
  pivot_longer(
    cols = c(mean_pm25, median_pm25),
    names_to = "statistic",
    values_to = "pm25"
  ) %>%
  mutate(
    statistic = recode(
      statistic,
      mean_pm25   = "Mean",
      median_pm25 = "Median"
    )
  )

p_year <- ggplot(yearly_long, aes(x = year, y = pm25, group = statistic, color = statistic)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.2) +
  scale_x_continuous(
    breaks = yearly_pm25$year,
    expand = c(0.01, 0.01)
  ) +
  scale_y_continuous(
  breaks = scales::pretty_breaks(n = 6),
  expand = expansion(mult = c(0.02, 0.05))
  )+
  scale_color_manual(values = c("Mean" = "#2C7FB8", "Median" = "#F59E0B")) +
  labs(
    title = "Yearly Mean and Median PM2.5 Concentration in London",
    subtitle = "City-wide daily averages aggregated by year",
    x = NULL,
    y = expression(PM[2.5]~(mu*g/m^3)),
    color = NULL,
    caption = "Lines show yearly aggregates; mean vs median highlights skew/outliers"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text = element_text(color = "grey20"),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.1),

    legend.position = "top",
    legend.direction = "horizontal",
    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(12, 14, 12, 12)
  )

ggsave(
  filename = file.path(out_dir_plot, "Fig5_yearly_trend_mean_median.png"),
  plot = p_year,
  width = 8,
  height = 4.5,
  dpi = 300
)


# Statistical Analysis: Long-Term Differences in PM2.5 Levels Across Years
df %>%
  mutate(year = year(date)) %>%
  kruskal.test(pm25_london_mean ~ year, data = .)

# =========================================================
# Output:
#
#        Kruskal-Wallis rank sum test
#
# data:  pm25_london_mean by year
# Kruskal-Wallis chi-squared = 361.93, df = 9, p-value < 2.2e-16
# =========================================================



# Monthly Summary
monthly_summary <- df %>%
  mutate(month = month(date, label = TRUE, abbr = TRUE)) %>%
  group_by(month) %>%
  summarise(
    mean_pm25   = mean(pm25_london_mean, na.rm = TRUE),
    median_pm25 = median(pm25_london_mean, na.rm = TRUE),
    sd_pm25     = sd(pm25_london_mean, na.rm = TRUE),
    iqr_pm25    = IQR(pm25_london_mean, na.rm = TRUE),
    n_days      = sum(!is.na(pm25_london_mean)),
    .groups = "drop"
  )

write_csv(
  monthly_summary,
  file.path(out_dir_table, "Table2_monthly_pm25_summary.csv")
)


# 30-day Rolling Mean of PM2.5 in London
df <- df %>%
  arrange(date) %>%
  mutate(
    pm25_roll30 = rollmean(
      pm25_london_mean,
      k = 30,
      fill = NA,
      align = "right"
    )
  )

p_roll <- ggplot(df, aes(x = date, y = pm25_roll30)) +
  geom_line(
    color = "#da8d1a",
    linewidth = 0.8,
    alpha = 0.95
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    expand = c(0.01, 0.01)
  ) +
  labs(
    title = "30-day Rolling Mean of PM2.5 in London",
    subtitle = "Smoothed long-term trend based on city-wide daily averages",
    x = NULL,
    y = expression(PM[2.5]~(mu*g/m^3)),
    caption = "Rolling window: 30 days"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text = element_text(color = "grey20"),
    axis.title.y = element_text(margin = margin(r = 8)),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),

    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.2
    ),

    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(12, 18, 12, 12)
  )

ggsave(
  filename = file.path(out_dir_plot, "Fig6_rolling_mean_30days.png"),
  plot = p_roll,
  width = 11,
  height = 4.5,
  dpi = 300
)



# Statistical Analysis: Spearman correlation between Time and PM2.5
df_spear <- df %>%
  select(date, pm25_london_mean) %>%
  filter(!is.na(date), !is.na(pm25_london_mean)) %>%
  mutate(time_num = as.numeric(date)) 

spear_res <- cor.test(
  x = df_spear$time_num,
  y = df_spear$pm25_london_mean,
  method = "spearman",
  exact = FALSE
)

spear_res
# =========================================================
# Output:
#
#        Spearman's rank correlation rho
#
# data:  df_spear$time_num and df_spear$pm25_london_mean
# S = 9017942471, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
#        rho
# -0.3009894
# =========================================================


