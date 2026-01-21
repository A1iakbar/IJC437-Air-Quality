# ============================================================
# IJC437 - Introduction to Data Science
# Script: 00_setup_packages.R
#
# Purpose:
# - Install and load all required R packages for the project.
# - Run once before executing the analysis scripts.
# ============================================================

required_packages <- c(
  "tidyverse",
  "lubridate",
  "httr",
  "jsonlite",
  "janitor",
  "ggplot2",
  "patchwork",
  "zoo",
  "caret",
  "randomForest",
  "xgboost",
  "Metrics",
  "dplyr",
  "readr",
  "purr",
  "tidyr",
  "tibble",
  "scales",
  "forecast",
  "Hmisc",
  "tidymodels",
  "rsample",
  "ranger",
  "bonsai"
)





# Load packages
invisible(lapply(required_packages, library, character.only = TRUE))

