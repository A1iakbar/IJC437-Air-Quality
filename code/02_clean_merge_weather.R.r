# ============================================================
# IJC437 - Introduction to Data Science (Individual Coursework)
# Script: 02_clean_merge_weather.R
#
# Purpose:
# - Prepare an analysis-ready daily dataset by combining:
#   (i) city-wide daily PM2.5 metrics (from Script 01),
#   (ii) daily meteorological variables from Open-Meteo.
# - Produce a merged dataset used across EDA and modelling scripts.
#
# Data sources:
# - PM2.5: data/processed/primary_pm25_daily_london.csv (OpenAQ-derived output from Script 01)
# - Weather: Open-Meteo Archive API (Central London coordinates)
#
# Key steps:
# 1) Load the primary daily PM2.5 dataset and extract its date range.
# 2) Download daily weather variables (temperature, wind speed, precipitation) from Open-Meteo
#    for the same period and build a tidy weather table.
# 3) Merge PM2.5 and weather by date (left join) to preserve the PM2.5 time index.
# 4) Create a compact data-quality summary (missingness + basic PM2.5 stats).
#
# Outputs (written to disk):
# - data/processed/openmeteo_daily_london.csv
# - output/tables/Table1_data_quality_summary.csv
# - data/processed/merged_pm25_weather.csv
#
# Reproducibility notes:
# - Uses explicit date alignment (no interpolation).
# - Fails fast if Open-Meteo response structure is unexpected.
#
# ============================================================


# ==================================================
# Installing and Importing Necessary Libraries
# ==================================================
library(dplyr)
library(readr)
library(lubridate)
library(httr)
library(jsonlite)
library(tidyr)
library(tibble)


# -------------------------------
# Paths + Directories
# -------------------------------

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)


# -------------------------------
# Loading primary PM2.5 dataset
# -------------------------------

pm25 <- readr::read_csv("data/processed/primary_pm25_daily_london.csv", show_col_types = FALSE)


pm25 <- pm25 %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

start_date <- min(pm25$date, na.rm = TRUE)
end_date   <- max(pm25$date, na.rm = TRUE)



# -------------------------------
# Downloading daily weather data from Open-Meteo
# -------------------------------
# Central London coordinates
LAT <- 51.5074
LON <- -0.1278


DAILY_VARS <- c("temperature_2m_mean", "wind_speed_10m_mean", "precipitation_sum")

# Open-Meteo Archive API endpoint
OPEN_METEO_URL <- "https://archive-api.open-meteo.com/v1/archive"

query <- list(
  latitude  = LAT,
  longitude = LON,
  start_date = as.character(start_date),
  end_date   = as.character(end_date),
  daily = paste(DAILY_VARS, collapse = ","),
  timezone = "Europe/London"
)


res <- httr::GET(OPEN_METEO_URL, query = query)
txt <- httr::content(res, "text", encoding = "UTF-8")

if (httr::http_error(res)) {
  stop("Open-Meteo request failed. HTTP ", httr::status_code(res), " -> ", substr(txt, 1, 500))
}

weather_json <- jsonlite::fromJSON(txt, flatten = TRUE)

# Validating expected structure
if (is.null(weather_json$daily) || is.null(weather_json$daily$time)) {
  stop("Unexpected Open-Meteo response: 'daily.time' is missing.")
}

# -------------------------------
# Building tidy weather dataframe
# -------------------------------
weather_daily <- tibble(
  date = as.Date(weather_json$daily$time),
  temp_mean_c = weather_json$daily$temperature_2m_mean,
  wind_mean_kmh = weather_json$daily$wind_speed_10m_mean,
  prcp_sum_mm = weather_json$daily$precipitation_sum
) %>%
  arrange(date)


# Saving raw weather data 
readr::write_csv(weather_daily, "data/processed/openmeteo_daily_london.csv")

# -------------------------------
# Merging PM2.5 + weather
# -------------------------------
merged <- pm25 %>%
  left_join(weather_daily, by = "date") %>%
  arrange(date)

# -------------------------------
# Data quality summary
# -------------------------------
qa_summary <- tibble(
  metric = c(
    "n_days_total",
    "n_days_pm25_nonNA",
    "n_days_weather_temp_nonNA",
    "n_days_weather_wind_nonNA",
    "n_days_weather_prcp_nonNA",
    "n_days_complete_cases_pm25_weather",
    "date_min",
    "date_max",
    "pm25_mean",
    "pm25_sd",
    "median_active_sensors"
  ),
  value = c(
    nrow(merged),
    sum(!is.na(merged$pm25_london_mean)),
    sum(!is.na(merged$temp_mean_c)),
    sum(!is.na(merged$wind_mean_kmh)),
    sum(!is.na(merged$prcp_sum_mm)),
    sum(complete.cases(merged %>% select(pm25_london_mean, temp_mean_c, wind_mean_kmh, prcp_sum_mm))),
    as.character(min(merged$date)),
    as.character(max(merged$date)),
    as.character(mean(merged$pm25_london_mean, na.rm = TRUE)),
    as.character(sd(merged$pm25_london_mean, na.rm = TRUE)),
    as.character(median(merged$n_sensors, na.rm = TRUE))
  )
)

readr::write_csv(qa_summary, "output/tables/Table1_data_quality_summary.csv")


# -------------------------------
# Saving merged dataset
# -------------------------------
readr::write_csv(merged, "data/processed/merged_pm25_weather.csv")
