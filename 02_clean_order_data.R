# 02_clean_order_data.R
# ────────────────────────────────────────────────────────────────
# Cleans and preprocesses the JD order dataset
# ────────────────────────────────────────────────────────────────
# Load global setup (libraries + data_path)
source("00_setup.R")

# Load required libraries only
library(data.table)
library(readr)
library(lubridate)
library(dplyr)

# Load data
order_raw <- read_csv(glue("{data_path}/JD_order_data.csv"))

# Rename timestamp column and convert to POSIXct and numeric
order_raw <- order_raw %>%
  rename(request_time = order_time) %>%
  mutate(
    request_time = as.POSIXct(request_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    request_time_numeric = as.numeric(request_time)
  )

# Clean placeholder values
order_clean <- order_raw %>%
  mutate(
    across(where(is.numeric), ~ ifelse(. == -1, NA, .)),
    across(where(is.character), ~ ifelse(. %in% c("-", "U"), NA, .))
  )

# Create time-based features
order_clean <- order_clean %>%
  mutate(
    day_of_week = wday(request_time),
    day_of_month = day(request_time),
    hour = hour(request_time),
    action = "order"
  )


# Convert to data.table for aggregation
order_clean <- as.data.table(order_clean)
setorder(order_clean, user_ID, request_time_numeric)

# Compute order_clean features
order_clean <- order_clean %>%
  group_by(sku_ID, day_of_month) %>%
  mutate(
    NO_sales_within_day = n(),
    number_of_unique_users_orderd_daily = n_distinct(user_ID),
    .groups = "drop"
  )

# Convert to data.table for downstream compatibility
setDT(order_clean)

# Save cleaned version to disk
write_csv(order_clean, glue("{data_path}/order_after_cleaning.csv"))
