# 04_click_merge_balanced.R
# ——————————————————————————————————
# Loads cleaned click data, processes click counts, merges with balanced panel
# ————————————————————————————————

# Load setup
source("00_setup.R")

# Load cleaned balanced panel
balanced_panel <- read_csv(glue("{data_path}/balanced_panel.csv"))

# Load click data
click <- read_csv(glue("{data_path}/JD_click_data.csv"))

# Clean click dataset
click <- click %>%
  mutate(
    across(where(is.numeric), ~ na_if(., -1)),
    across(where(is.character), ~ na_if(.x, "-")),
    across(where(is.character), ~ na_if(.x, "U")),
    request_time = as.POSIXct(request_time, format="%Y-%m-%d %H:%M:%S", tz = "UTC"),
    request_time_numeric = as.numeric(request_time),
    day_of_week = lubridate::wday(request_time),
    day_of_month = lubridate::day(request_time),
    hour = lubridate::hour(request_time),
    action = "click"
  )

# Convert to data.table for aggregation
click <- as.data.table(click)
setorder(click, user_ID, request_time_numeric)

# Compute click features
click_summary <- click %>%
  group_by(sku_ID, day_of_month) %>%
  summarise(
    click_including_refresh_daily = n(),
    number_of_unique_users_clicked_daily = n_distinct(user_ID),
    .groups = "drop"
  )

# Merge click features into balanced panel
balanced_panel_clicks <- balanced_panel %>%
  left_join(click_summary, by = c("sku_ID", "day_of_month")) %>%
  mutate(
    click_including_refresh_daily = replace_na(click_including_refresh_daily, 0),
    number_of_unique_users_clicked_daily = replace_na(number_of_unique_users_clicked_daily, 0)
  )

# Save enriched balanced panel
write_csv(balanced_panel_clicks, glue("{data_path}/balanced_panel_with_clicks_and_order_sku.csv"))