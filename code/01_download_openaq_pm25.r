# ============================================================
# IJC437 - Introduction to Data Science (Individual Coursework)
# Script: 01_download_openaq_pm25.R
#
# Purpose:
# - Download daily PM2.5 data for London-area monitoring locations from OpenAQ (v3 API).
# - Select fixed monitoring locations within 25km of central London and identify the best
#   PM2.5 sensor per location based on daily coverage.
# - Produce a city-level daily dataset (wide by sensor + London mean + sensor count).
#
# Data sources:
# - OpenAQ v3 API (requires OPENAQ_KEY environment variable).
#
# Key steps:
# 1) Query candidate locations in radius; keep fixed monitors (not mobile).
# 2) Probe each location to find PM2.5 sensor with best daily coverage (page-1 rows).
# 3) Select top N sensors by coverage and download full daily series (paginated).
# 4) Pivot to wide format and compute:
#    - pm25_london_mean (row mean across sensors)
#    - n_sensors (non-missing sensors per day)
#
# Inputs / requirements:
# - OPENAQ_KEY must be set (e.g., Sys.setenv(OPENAQ_KEY="...") or via .Renviron).
#
# Outputs (written to disk):
# - output/tables/Table0_all_candidate_locations_sensor_probe.csv
# - output/tables/Table0_selected_locations_by_coverage.csv
# - data/processed/primary_pm25_daily_london.csv
#
# Reproducibility notes:
# - Uses retry + exponential backoff for transient API errors (e.g., 429/5xx).
# - Includes short sleeps to reduce rate-limit risk.
#
# ============================================================

# ==================================================
# Importing Necessary Libraries
# ==================================================
library(lubridate)
library(jsonlite)
library(tibble)
library(tidyr)
library(dplyr)
library(readr)
library(purrr)
library(httr)



# -------------------------------
# Directories
# -------------------------------
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)


# -------------------------------
# API key (environment variable)
# -------------------------------

# Please Insert Your Own API Key!!!
OPENAQ_KEY <- "42178ba929787ed7e26cf20cdefbcf874b106e51181dd113a9418dcf222f1c8b"


# -------------------------------
# OpenAQ GET helper
# -------------------------------
openaq_get <- function(path, query = list()) {
  url <- paste0("https://api.openaq.org/v3", path)
  res <- GET(url, add_headers(`X-API-Key` = OPENAQ_KEY), query = query)
  txt <- content(res, "text", encoding = "UTF-8")
  if (http_error(res)) stop("HTTP ", status_code(res), " -> ", txt)
  fromJSON(txt, flatten = TRUE)
}

# Retry wrapper with exponential backoff 
safe_openaq_get <- function(path, query = list(), retries = 6, base_sleep = 1) {
  for (i in seq_len(retries)) {
    out <- tryCatch(openaq_get(path, query = query), error = function(e) e)
    if (!inherits(out, "error")) return(out)

    msg <- conditionMessage(out)
    retryable <- grepl("HTTP (500|502|503|504|429)", msg)

    if (!retryable) stop(out)

    Sys.sleep(base_sleep * (2^(i - 1)) + runif(1, 0, 0.4))
  }
  return(NULL)
}

# -------------------------------
# Parameters
# -------------------------------
LAT <- 51.5074
LON <- -0.1278
RADIUS_M <- 25000

N_LOCATIONS <- 25
PROBE_DATE_FROM <- "2016-01-01"
PROBE_DATE_TO   <- "2025-12-15"

DOWNLOAD_DATE_FROM <- "2016-01-01"
DOWNLOAD_DATE_TO   <- "2025-12-31"

# -------------------------------
# Getting locations in radius
# -------------------------------
locs_london <- safe_openaq_get("/locations", query = list(
  coordinates = paste0(LAT, ",", LON),
  radius = RADIUS_M,
  country = "GB",
  limit = 100,
  page = 1
))

locs_london_df <- jsonlite::fromJSON(jsonlite::toJSON(locs_london$results), flatten = TRUE) %>%
  dplyr::select(
    id, name, locality, timezone, country.code,
    datetimeFirst.utc, datetimeLast.utc,
    isMonitor, isMobile
  ) %>%
  dplyr::arrange(desc(datetimeLast.utc))

cat("All locations returned:", nrow(locs_london_df), "\n")

# NA-safe fixed monitor selection
sel_locs <- locs_london_df %>%
  mutate(
    isMonitor = as.logical(isMonitor),
    isMobile  = as.logical(isMobile)
  ) %>%
  filter(
    isMonitor == TRUE,
    is.na(isMobile) | isMobile == FALSE
  ) %>%
  select(id, name, locality, datetimeFirst.utc, datetimeLast.utc)

cat("Fixed candidate locations:", nrow(sel_locs), "\n")

if (nrow(sel_locs) == 0) {
  stop("No fixed candidate locations found. Check OpenAQ response fields isMonitor/isMobile.")
}

loc_ids_vec <- sel_locs$id

# -------------------------------
# Probe best PM2.5 sensor per location
# -------------------------------
probe_loc_pm25 <- function(loc_id) {

  s <- safe_openaq_get(
    paste0("/locations/", loc_id, "/sensors"),
    query = list(limit = 500, page = 1)
  )

  if (is.null(s) || is.null(s$results) || length(s$results) == 0) {
    return(tibble(
      loc_id = loc_id,
      pm25_sensor_id = NA_integer_,
      page1_rows = 0,
      min_date = NA_character_,
      max_date = NA_character_,
      status = "sensors_failed_or_empty"
    ))
  }

  sdf <- fromJSON(toJSON(s$results), flatten = TRUE) %>%
    select(id, parameter.name, name)

  pm <- sdf %>% filter(parameter.name == "pm25")

  if (nrow(pm) == 0) {
    return(tibble(
      loc_id = loc_id,
      pm25_sensor_id = NA_integer_,
      page1_rows = 0,
      min_date = NA_character_,
      max_date = NA_character_,
      status = "no_pm25_sensor"
    ))
  }

  res_list <- lapply(pm$id, function(sensor_id) {
    Sys.sleep(0.35)

    r <- safe_openaq_get(
      paste0("/sensors/", sensor_id, "/days"),
      query = list(
        date_from = PROBE_DATE_FROM,
        date_to   = PROBE_DATE_TO,
        limit = 1000,
        page = 1
      )
    )

    if (is.null(r) || is.null(r$results)) {
      return(tibble(
        loc_id = loc_id,
        pm25_sensor_id = sensor_id,
        page1_rows = 0,
        min_date = NA_character_,
        max_date = NA_character_,
        status = "days_failed"
      ))
    }

    df <- fromJSON(toJSON(r$results), flatten = TRUE)

    if (nrow(df) == 0) {
      return(tibble(
        loc_id = loc_id,
        pm25_sensor_id = sensor_id,
        page1_rows = 0,
        min_date = NA_character_,
        max_date = NA_character_,
        status = "days_empty"
      ))
    }

    date_col <- dplyr::coalesce(
      df$period.datetimeFrom.local,
      df$period.datetimeFrom.utc
    )

    dd <- tibble(date = as.Date(date_col)) %>% arrange(date)

    tibble(
      loc_id = loc_id,
      pm25_sensor_id = sensor_id,
      page1_rows = nrow(df),
      min_date = as.character(min(dd$date, na.rm = TRUE)),
      max_date = as.character(max(dd$date, na.rm = TRUE)),
      status = "ok"
    )
  })

  bind_rows(res_list) %>%
    arrange(desc(page1_rows)) %>%
    slice(1)
}

scan <- map_dfr(loc_ids_vec, function(id) {
  tryCatch(
    probe_loc_pm25(id),
    error = function(e) tibble(
      loc_id = id,
      pm25_sensor_id = NA_integer_,
      page1_rows = 0,
      min_date = NA_character_,
      max_date = NA_character_,
      status = paste0("probe_error: ", conditionMessage(e))
    )
  )
}) %>% arrange(desc(page1_rows))

cat("Probe table rows:", nrow(scan), "\n")
print(table(scan$status, useNA = "ifany"))

write_csv(scan, "output/tables/Table0_all_candidate_locations_sensor_probe.csv")

# Selecting top N by data coverage (fail-safe: allowing any status as long as rows exist)
selected_locations <- scan %>%
  filter(!is.na(pm25_sensor_id), page1_rows > 0) %>%
  arrange(desc(page1_rows)) %>%
  slice(1:min(N_LOCATIONS, n()))

cat("Selected locations:", nrow(selected_locations), "\n")
print(selected_locations)


write_csv(selected_locations, "output/tables/Table0_selected_locations_by_coverage.csv")

sensor_ids <- selected_locations$pm25_sensor_id
cat("Number of selected sensors:", length(sensor_ids), "\n")

# -------------------------------
# Downloading full daily series for selected sensors (pagination)
# -------------------------------

fetch_all_days <- function(sensor_id,
                           date_from = DOWNLOAD_DATE_FROM,
                           date_to   = DOWNLOAD_DATE_TO,
                           sleep_sec = 0.25) {
  page <- 1
  out <- list()

  repeat {
    res <- safe_openaq_get(
      path = paste0("/sensors/", sensor_id, "/days"),
      query = list(
        date_from = date_from,
        date_to   = date_to,
        limit = 1000,
        page  = page
      )
    )

    if (is.null(res) || is.null(res$results) || length(res$results) == 0) break

    df <- tryCatch(
      tibble::as_tibble(res$results),
      error = function(e) NULL
    )

    if (is.null(df) || nrow(df) == 0) break
    if (!("value" %in% names(df))) break

    # local datetime if available
    date_col <- dplyr::coalesce(
      df$period.datetimeFrom.local,
      df$period.datetimeFrom.utc
    )

    if (is.null(date_col) || length(date_col) == 0 || all(is.na(date_col))) break

    out[[page]] <- tibble::tibble(
      sensor_id = sensor_id,
      date      = as.Date(date_col),
      pm25      = df$value
    )

    # Stopping when the page is not full
    if (nrow(df) < 1000) break

    page <- page + 1
    Sys.sleep(sleep_sec)
  }

  dplyr::bind_rows(out)
}



pm25_list_full <- lapply(sensor_ids, fetch_all_days)
pm25_all_full  <- bind_rows(pm25_list_full)

cat("Total rows downloaded:", nrow(pm25_all_full), "\n")

# -------------------------------
# City-level daily dataset (wide + London mean)
# -------------------------------
pm25_wide <- pm25_all_full %>%
  group_by(sensor_id, date) %>%
  summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(
    names_from = sensor_id,
    values_from = pm25,
    names_prefix = "pm25_"
  ) %>%
  arrange(date)

pm25_wide <- pm25_wide %>%
  mutate(
    pm25_london_mean = rowMeans(select(., starts_with("pm25_")), na.rm = TRUE),
    n_sensors = rowSums(!is.na(select(., starts_with("pm25_"))))
  )

cat("Final dataset rows:", nrow(pm25_wide), "\n")

# -------------------------------
# Saving
# -------------------------------
write_csv(
  pm25_wide,
  "data/processed/primary_pm25_daily_london.csv"
)

