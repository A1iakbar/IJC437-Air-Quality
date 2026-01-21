# ============================================================
# IJC437 - Introduction to Data Science
# Script: 00_setup_packages.R
#
# Purpose:
# - Install all required R packages for the project.
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


# Installing all required packages
install.packages(setdiff(required_packages, installed.packages()[, "Package"]))
