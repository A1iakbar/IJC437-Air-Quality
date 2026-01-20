# ==================================================
# Installing and Importing Necessary Libraries
# ==================================================
library(tidyverse)
library(lubridate)
library(Hmisc)
library(scales)



# ==================================================
# Loading Merged Data and Basic Setup
# ==================================================
out_dir_plot <- "output/figures"
out_dir_table <- "output/tables"
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)


df <- read_csv("data/processed/merged_pm25_weather.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

# Keeping only RQ2 columns + drop rows with missing values for correlation/scatter
df_rq2 <- df %>%
  select(date, pm25_london_mean, temp_mean_c, wind_mean_kmh, prcp_sum_mm) %>%
  filter(
    !is.na(pm25_london_mean),
    !is.na(temp_mean_c),
    !is.na(wind_mean_kmh),
    !is.na(prcp_sum_mm)
  )


# ==================================================
# Spearman correlations (rho + p-values)
# ==================================================

# Correlation matrix (rho)
rho_mat <- cor(
  df_rq2 %>% select(pm25_london_mean, temp_mean_c, wind_mean_kmh, prcp_sum_mm),
  method = "spearman",
  use = "complete.obs"
)
rho_mat

# Correlations + p-values (Hmisc)
rc <- rcorr(
  as.matrix(df_rq2 %>% select(pm25_london_mean, temp_mean_c, wind_mean_kmh, prcp_sum_mm)),
  type = "spearman"
)
rc 

cor_table <- tibble(
  variable = c("temp_mean_c", "wind_mean_kmh", "prcp_sum_mm"),
  spearman_rho = c(
    rc$r["pm25_london_mean","temp_mean_c"],
    rc$r["pm25_london_mean","wind_mean_kmh"],
    rc$r["pm25_london_mean","prcp_sum_mm"]
  ),
  p_value = c(
    rc$P["pm25_london_mean","temp_mean_c"],
    rc$P["pm25_london_mean","wind_mean_kmh"],
    rc$P["pm25_london_mean","prcp_sum_mm"]
  ),
  n = c(
    rc$n["pm25_london_mean","temp_mean_c"],
    rc$n["pm25_london_mean","wind_mean_kmh"],
    rc$n["pm25_london_mean","prcp_sum_mm"]
  )
) %>%
  mutate(
    spearman_rho = round(spearman_rho, 3),
    p_value = format.pval(p_value, digits = 3)
  )

cor_table
write_csv(cor_table, file.path(out_dir_table, "Table3_spearman_correlations.csv"))


# ==================================================
# Scatter plots PM25 vs Weather Variables
# ==================================================

base_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.1),
    panel.grid.minor = element_blank()
  )

# PM2.5 vs windspeed
p_wind <- ggplot(df_rq2, aes(x = wind_mean_kmh, y = pm25_london_mean)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "PM2.5 vs wind speed",
    x = "Wind speed (km/h)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  base_theme

ggsave(
  filename = file.path(out_dir_plot, "Fig7_pm25_vs_windspeed.png"),
  plot = p_wind, width = 8, height = 4.5, dpi = 300
)

# PM2.5 vs temperature
p_temp <- ggplot(df_rq2, aes(x = temp_mean_c, y = pm25_london_mean)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "PM2.5 vs temperature",
    x = "Temperature (°C)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  base_theme

ggsave(
  filename = file.path(out_dir_plot, "Fig8_pm25_vs_temperature.png"),
  plot = p_temp, width = 8, height = 4.5, dpi = 300
)

# PM2.5 vs precipitation
p_prcp <- ggplot(df_rq2, aes(x = prcp_sum_mm, y = pm25_london_mean)) +
  geom_point(alpha = 0.25, size = 0.8) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(
    title = "PM2.5 vs precipitation",
    x = "Precipitation (mm)",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  base_theme

ggsave(
  filename = file.path(out_dir_plot, "Fig9_pm25_vs_precipitation.png"),
  plot = p_prcp, width = 8, height = 4.5, dpi = 300
)



# ==================================================
# Conditional Distributions Wind Groups
# ==================================================

# Creating wind speed tertiles (Low/Medium/High)
df_rq2b <- df_rq2 %>%
  mutate(
    wind_group = ntile(wind_mean_kmh, 3),
    wind_group = factor(
      wind_group,
      levels = c(1, 2, 3),
      labels = c("Low wind", "Medium wind", "High wind")
    )
  )

# Checking group sizes
df_rq2b %>% count(wind_group)

# Boxplot: PM2.5 distribution by wind category
p_wind_box <- ggplot(df_rq2b, aes(x = wind_group, y = pm25_london_mean)) +
  geom_boxplot(fill = "#de9030", color = "black", outlier.alpha = 0.3) +
  labs(
    title = "PM2.5 under different wind conditions",
    subtitle = "Wind speed grouped into tertiles (low/medium/high)",
    x = "Wind speed category",
    y = expression(PM[2.5]~(mu*g/m^3))
  ) +
  base_theme

ggsave(
  filename = file.path(out_dir_plot, "Fig10_pm25_by_wind_categories.png"),
  plot = p_wind_box, width = 8, height = 4.5, dpi = 300
)

# Summary stats by wind group
wind_summary <- df_rq2b %>%
  group_by(wind_group) %>%
  summarise(
    n_days = n(),
    mean_pm25 = mean(pm25_london_mean),
    median_pm25 = median(pm25_london_mean),
    iqr_pm25 = IQR(pm25_london_mean),
    .groups = "drop"
  ) %>%
  mutate(
    mean_pm25 = round(mean_pm25, 2),
    median_pm25 = round(median_pm25, 2),
    iqr_pm25 = round(iqr_pm25, 2)
  )

wind_summary
write_csv(wind_summary, file.path(out_dir_table, "Table4_wind_group_summary.csv"))



# ==================================================
# Statistical test (Kruskal–Wallis) for Wind Groups
# ==================================================

# Test whether PM2.5 distributions differ across wind categories
kw_wind <- kruskal.test(pm25_london_mean ~ wind_group, data = df_rq2b)
kw_wind

# =========================================================
# Output:
#
#        Kruskal-Wallis rank sum test
#
# data:  pm25_london_mean by wind_group
# Kruskal-Wallis chi-squared = 374.27, df = 2, p-value < 2.2e-16
# 
# =========================================================
