# ============================================================
# IJC437 - Introduction to Data Science (Individual Coursework)
# Script: 05_rq3_prediction_models.R
#
# Purpose:
# - Address RQ3 by modelling and predicting daily PM2.5 concentrations in London.
# - Evaluate how much predictive signal is provided by:
#   (i) temporal persistence (lagged PM2.5 and rolling means),
#   (ii) meteorological variables (temperature, wind speed, precipitation),
#   (iii) calendar-based features (month, day-of-week).
#
# Input data:
# - data/processed/merged_pm25_weather.csv
#   (daily London PM2.5 + meteorological variables; produced in Script 02)
#
# Feature engineering:
# - Temporal persistence:
#   * pm25_lag_1  (1-day lag)
#   * pm25_roll7  (7-day rolling mean)
#   * pm25_roll30 (30-day rolling mean)
# - Calendar structure:
#   * month
#   * day_of_week
#
# Modelling strategy:
# - Supervised regression with a strict chronological split:
#   * First 80% of observations used for training
#   * Last 20% reserved as a holdout test set
# - Models evaluated:
#   * Naive baseline (lag-1 persistence)
#   * Linear regression
#   * Random Forest
#   * XGBoost
# - Tree-based models are used to capture non-linear relationships
#   and interactions identified during exploratory analysis.
#
# Hyperparameter tuning:
# - Random Forest hyperparameters (mtry, min_n) are tuned using
#   rolling-origin cross-validation on the training data only.
# - The final tuned model is evaluated once on the holdout test set
#   to avoid information leakage.
#
# Evaluation metrics:
# - RMSE, MAE, and R² computed on the test set.
#
# Outputs (written to disk):
# - output/tables/Table5_metrics_comparison.csv
# - output/tables/Table6_final_model_metrics.csv
# - output/tables/Table7_test_set_actual_vs_predicted.csv
# - output/figures/Fig11_test_actual_vs_pred_rf.png
# - output/figures/Fig12_error_over_time_rf.png
# - output/figures/Fig13_rf_feature_importance.png
#
# Reproducibility notes:
# - Uses a fixed random seed (set.seed).
# - All results are written to disk; no interactive steps required.
# - Chronological splitting ensures realistic forecasting conditions.
#
# ============================================================



# ==================================================
# Installing and Importing Necessary Libraries
# ==================================================

#install.packages(c("ranger", "xgboost"))
#if (!requireNamespace("bonsai", quietly = TRUE)) install.packages("bonsai")
#if (!requireNamespace("lightgbm", quietly = TRUE)) install.packages("lightgbm")
library(tidymodels)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(rsample)
library(ranger)
library(bonsai)
library(tibble)
library(dplyr)
library(tidyr)
library(zoo)
set.seed(42)


# ==================================================
# Loading Dataset and Data Preprocessing
# ==================================================
out_dir_plot <- "output/figures"
out_dir_table <- "output/tables"
dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables", recursive = TRUE, showWarnings = FALSE)


# Reading the data
df <- read_csv("data/processed/merged_pm25_weather.csv")


# Converting date column to Date format and ensuring chronological order
df <- df %>%
  mutate(date = as.Date(date)) %>%
  arrange(date)


# Creating Lag, Month, Day_of_Week Variables
df_model <- df %>%
  mutate(
    pm25_lag_1  = lag(pm25_london_mean, 1),
    pm25_roll7  = rollmean(
      pm25_london_mean,
      k = 7,
      fill = NA,
      align = "right"
    ),
    pm25_roll30 = rollmean(
      pm25_london_mean,
      k = 30,
      fill = NA,
      align = "right"
    ),
    month       = month(date),
    day_of_week = wday(date, label = TRUE)
  ) %>%
  select(
    date,
    pm25_london_mean,
    pm25_lag_1,
    pm25_roll7,
    pm25_roll30,
    temp_mean_c,
    wind_mean_kmh,
    prcp_sum_mm,
    month,
    day_of_week
  )


# Dropping NA values after creating lag values
df_model <- df_model %>% drop_na()




# ==================================================
# Modelling
# ==================================================

# Spliting Index
split_index <- floor(0.8 * nrow(df_model))

train_data <- df_model[1:split_index, ]
test_data  <- df_model[(split_index + 1):nrow(df_model), ]



# Baseline: Linear Regression
lm_spec <- linear_reg() %>%
  set_engine("lm")


lm_fit <- lm_spec %>%
  fit(pm25_london_mean ~ pm25_lag_1 + pm25_roll7 + pm25_roll30 +
        temp_mean_c + wind_mean_kmh + prcp_sum_mm +
        factor(month) + day_of_week,
      data = train_data)

lm_preds <- predict(lm_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(date, pm25_london_mean)) %>%
  rename(pred = .pred, actual = pm25_london_mean)

# Metrics
lm_metrics <- lm_preds %>%
  metrics(truth = actual, estimate = pred) %>%
  filter(.metric %in% c("rmse", "mae", "rsq")) %>%
  mutate(model = "Linear Regression")



# Random Forest Model
rf_spec <- rand_forest(
  trees = 500,
  mtry  = 4,
  min_n = 10
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_fit <- rf_spec %>%
  fit(
    pm25_london_mean ~ pm25_lag_1 + pm25_roll7 + pm25_roll30 +
      temp_mean_c + wind_mean_kmh + prcp_sum_mm +
      factor(month) + day_of_week,
    data = train_data
  )

rf_preds <- predict(rf_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(date, pm25_london_mean)) %>%
  rename(pred = .pred, actual = pm25_london_mean)

rf_metrics <- rf_preds %>%
  metrics(truth = actual, estimate = pred) %>%
  filter(.metric %in% c("rmse", "mae", "rsq")) %>%
  mutate(model = "Random Forest")




# XGBoost Model
xgb_spec <- boost_tree(
  trees = 800,
  tree_depth = 6,
  learn_rate = 0.05,
  loss_reduction = 0,
  sample_size = 0.8,
  mtry = 10,
  min_n = 10
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_recipe <- recipe(
  pm25_london_mean ~ pm25_lag_1 + pm25_roll7 + pm25_roll30 +
    temp_mean_c + wind_mean_kmh + prcp_sum_mm +
    month + day_of_week,
  data = train_data
) %>%
  step_mutate(
    month = as.factor(month),
    day_of_week = as.factor(day_of_week)
  ) %>%
  step_dummy(all_nominal_predictors())

xgb_wf <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(xgb_spec)

xgb_fit <- fit(xgb_wf, data = train_data)

xgb_preds <- predict(xgb_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(date, pm25_london_mean)) %>%
  rename(pred = .pred, actual = pm25_london_mean)

xgb_metrics <- xgb_preds %>%
  metrics(truth = actual, estimate = pred) %>%
  filter(.metric %in% c("rmse", "mae", "rsq")) %>%
  mutate(model = "XGB")


# Naive preds
naive_preds <- test_data %>%
  mutate(pred = pm25_lag_1, actual = pm25_london_mean)

naive_metrics <- naive_preds %>%
  metrics(truth = actual, estimate = pred) %>%
  filter(.metric %in% c("rmse", "mae", "rsq")) %>%
  mutate(model = "Naive (lag-1)")

# Metrics Comparison for each Model
metrics_csv <- bind_rows(lm_metrics, rf_metrics, xgb_metrics, naive_metrics) %>%
  select(model, .metric, .estimate)

write_csv(
  metrics_csv,
  file.path("output/tables", "Table5_metrics_comparison.csv")
)

# Random Forest Hiperparametric Optimization
rf_tune_spec <- rand_forest(
  trees = 500,
  mtry  = tune(),
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_recipe <- recipe(
  pm25_london_mean ~ pm25_lag_1 + pm25_roll7 + pm25_roll30 +
    temp_mean_c + wind_mean_kmh + prcp_sum_mm +
    month + day_of_week,
  data = train_data
) %>%
  step_mutate(month = as.factor(month))

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_tune_spec)

rf_grid <- grid_regular(
  mtry(range = c(2, 8)),
  min_n(range = c(5, 30)),
  levels = 4
)

rf_folds <- rsample::rolling_origin(
  train_data,
  initial = 1000,
  assess  = 365,
  skip    = 365,
  cumulative = FALSE
)

rf_tune_res <- tune_grid(
  rf_workflow,
  resamples = rf_folds,
  grid = rf_grid,
  metrics = metric_set(rmse)
)

rf_tune_res %>% collect_metrics()

# Training Random Forest Model Second Time by Using Best Params Found in Previous Step (Hyperparameter Optimization)

rf_final_spec <- rand_forest(
  trees = 500,
  mtry  = 4,
  min_n = 21
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

rf_final_fit <- rf_final_spec %>%
  fit(
    pm25_london_mean ~ pm25_lag_1 + pm25_roll7 + pm25_roll30 +
      temp_mean_c + wind_mean_kmh + prcp_sum_mm +
      factor(month) + day_of_week,
    data = train_data
  )

rf_final_preds <- predict(rf_final_fit, new_data = test_data) %>%
  bind_cols(test_data %>% select(date, pm25_london_mean)) %>%
  rename(pred = .pred, actual = pm25_london_mean)


rf_final_metrics_full <- rf_final_preds %>%
  metrics(truth = actual, estimate = pred) %>%
  filter(.metric %in% c("rmse", "mae", "rsq"))

rf_final_metrics_full

write_csv(
  rf_final_metrics_full %>% mutate(model = "Random Forest (tuned)"),
  file.path(out_dir_table, "Table6_final_model_metrics.csv")
)


# Prediction vs Actual Values Table
test_compare <- rf_final_preds %>%
  mutate(
    error = actual - pred,
    abs_error = abs(error)
  ) %>%
  arrange(date)

test_compare %>%
  summarise(
    n = n(),
    rmse = sqrt(mean((actual - pred)^2)),
    mae  = mean(abs(actual - pred)),
    mean_error = mean(error)
  )

test_table <- test_compare %>%
  select(date, actual, pred, error, abs_error)

write_csv(
  test_table,
  file.path(out_dir_table, "Table7_test_set_actual_vs_predicted.csv")
)


# Prediction vs Actual Values Graph
plot_df <- test_compare %>%
  select(date, actual, pred) %>%
  pivot_longer(cols = c(actual, pred), names_to = "series", values_to = "value")

p <- ggplot(plot_df, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 0.7) +
  labs(
    title = "Test set: PM2.5 actual vs predicted (Random Forest)",
    x = "Date",
    y = "PM2.5 (µg/m³)",
    color = ""
  ) +
  theme_minimal() +
  theme(
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.1
    )
  )
ggsave(
  file.path(out_dir_plot, "Fig11_test_actual_vs_pred_rf.png"),
  plot = p,
  width = 10,
  height = 4.8,
  dpi = 300
)


# Prediction Error over Time (Test set)
error_df <- rf_final_preds %>%
  mutate(error = actual - pred)

p_error <- ggplot(error_df, aes(x = date, y = error)) +
  geom_hline(
    yintercept = 0,
    color = "black",
    linewidth = 0.6,
    linetype = "dashed"
  ) +
  geom_line(
    color = "#B45309",
    linewidth = 0.8,
    alpha = 0.85
  ) +
  labs(
    title = "Prediction error over time (Random Forest)",
    subtitle = "Error = actual − predicted PM2.5 (test set)",
    x = NULL,
    y = expression(Error~(mu*g/m^3)),
    caption = "Positive values indicate underestimation; negative values indicate overestimation"
  ) +
  scale_x_date(
    date_breaks = "2 months",
    date_labels = "%b %Y",
    expand = c(0.01, 0.01)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "grey30"),
    axis.text = element_text(color = "grey20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(
      colour = "black",
      fill = NA,
      linewidth = 1.1
    ),

    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(12, 14, 12, 12)
  )

ggsave(
  file.path(out_dir_plot, "Fig12_error_over_time_rf.png"),
  plot = p_error,
  width = 11,
  height = 4.5,
  dpi = 300
)


# Feature Importance
train_data2 <- train_data %>%
  mutate(month = as.factor(month))

rf_imp_fit <- ranger(
  pm25_london_mean ~ pm25_lag_1 + pm25_roll7 + pm25_roll30 +
    temp_mean_c + wind_mean_kmh + prcp_sum_mm +
    month + day_of_week,
  data = train_data2,
  num.trees = 500,
  mtry = 4,
  min.node.size = 30,
  importance = "permutation",
  seed = 42
)

rf_importance <- tibble(
  feature = names(rf_imp_fit$variable.importance),
  importance = rf_imp_fit$variable.importance
) %>%
  arrange(desc(importance))


# Visualization of Feature Importance
p_imp <- ggplot(
  rf_importance,
  aes(x = reorder(feature, importance), y = importance)
) +
  geom_col(fill = "#de995d") +
  coord_flip() +
  labs(
    title = "Random Forest feature importance (permutation-based)",
    x = "",
    y = "Importance score"
  ) +
  theme_minimal()


ggsave(
  file.path(out_dir_plot, "Fig13_rf_feature_importance.png"),
  plot = p_imp,
  width = 8,
  height = 4.5,
  dpi = 300
)
