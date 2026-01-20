# ============================================================
# IJC437 - Introduction to Data Science (Individual Coursework)
# Script: 02_clean_merge_weather.R
#
# Purpose:
# - Download daily meteorological variables for London from Open-Meteo
#   (temperature, wind speed, precipitation).
# - Clean and validate the weather data.
# - Merge weather with the PRIMARY PM2.5 daily dataset created in:
#   code/01_download_openaq_pm25.R
#
# Research Questions supported:
# - RQ2: Association between meteorology and daily PM2.5; distribution changes under conditions.
# - RQ3: Prediction of daily PM2.5 using meteorology + calendar-based features.
#
# Inputs:
# - data/processed/primary_pm25_daily_london.csv
#
# Outputs (saved files):
# - data/raw/openmeteo_daily_london.csv
# - data/processed/merged_pm25_weather.csv
# - output/tables/Table1_data_quality_summary.csv
#
# Reproducibility notes:
# - Date range is derived automatically from the primary PM2.5 dataset.
# - No secrets required (Open-Meteo is open).
# - Script includes error handling + basic QA checks.
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
# 1) Load primary PM2.5 dataset
# -------------------------------

pm25 <- readr::read_csv("data/processed/primary_pm25_daily_london.csv", show_col_types = FALSE)


pm25 <- pm25 %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)

start_date <- min(pm25$date, na.rm = TRUE)
end_date   <- max(pm25$date, na.rm = TRUE)



# -------------------------------
# 2) Download daily weather data from Open-Meteo
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

# Validate expected structure
if (is.null(weather_json$daily) || is.null(weather_json$daily$time)) {
  stop("Unexpected Open-Meteo response: 'daily.time' is missing.")
}

# -------------------------------
# 3) Build tidy weather dataframe
# -------------------------------
weather_daily <- tibble(
  date = as.Date(weather_json$daily$time),
  temp_mean_c = weather_json$daily$temperature_2m_mean,
  wind_mean_kmh = weather_json$daily$wind_speed_10m_mean,
  prcp_sum_mm = weather_json$daily$precipitation_sum
) %>%
  arrange(date)


# Save raw weather 
readr::write_csv(weather_daily, "data/processed/openmeteo_daily_london.csv")

# -------------------------------
# 4) Merge PM2.5 + weather
# -------------------------------
merged <- pm25 %>%
  left_join(weather_daily, by = "date") %>%
  arrange(date)

# -------------------------------
# 5) QA / Data quality summary (100-band friendly)
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
# 6) Save merged analysis-ready dataset
# -------------------------------
readr::write_csv(merged, "data/processed/merged_pm25_weather.csv")
