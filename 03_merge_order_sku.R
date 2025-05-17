# 03_merge_order_sku.R
# ────────────────────────────────────────────────────
# Merges cleaned SKU and order data, creates imputed features and balanced panel
# ───────────────────────────────────────────────────

# Load setup and libraries
source("00_setup.R")
library(data.table)
library(dplyr)
library(lubridate)
library(readr)
library(zoo)

# Load cleaned datasets
sku_clean <- read_csv(glue("{data_path}/sku_after_cleaning.csv"))
order_clean <- read_csv(glue("{data_path}/order_after_cleaning.csv"))

# Remove SKUs with conflicting definitions
sku_duplicates <- sku_clean %>%
  group_by(sku_ID) %>%
  summarize(across(c(type, brand_ID, attribute1, attribute2, product_space), ~ n_distinct(.) > 1, .names = "multiple_{.col}")) %>%
  filter(if_any(starts_with("multiple_"), ~ .x))

sku_dedup <- sku_clean %>%
  filter(!sku_ID %in% sku_duplicates$sku_ID)

# Merge SKU and order data
sku_order <- sku_dedup %>%
  left_join(order_clean, by = c("sku_ID", "type")) %>%
  group_by(sku_ID, day_of_month) %>%
  mutate(
    NO_sales_within_day = sum(quantity, na.rm = TRUE),
    log_NO_sales_within_day = log1p(NO_sales_within_day),
    after_enter = if_else(!is.na(first_entry_date_firstPArty) & request_time_numeric >= as.numeric(first_entry_date_firstPArty), 1, 0),
    after_enter_brand_ID = if_else(!is.na(first_entry_date_firstPArtybrand_ID) & request_time_numeric >= as.numeric(first_entry_date_firstPArtybrand_ID), 1, 0)
  ) %>%
  ungroup()

# Impute price per SKU per day
setDT(sku_order)
setorder(sku_order, sku_ID, request_time_numeric)
sku_order[, imputed_ave_final_unit_price := mean(final_unit_price, na.rm = TRUE), by = .(sku_ID, day_of_month)]

# Save imputed dataset
write_csv(sku_order, glue("{data_path}/sku_order_imputed.csv"))

# Define columns to carry forward per SKU
cols_to_impute <- c(
  "attribute1", "attribute2", "brand_ID", "product_space", "enter_typ_one", 
  "first_party_entrance_time_day", "first_entry_date_firstPArty",
  "product_space_type_classification", "activate_date", "deactivate_date", 
  "enter_typ_one_within_brand", "n_sku_within_product_space", "n_sku_within_brand_ID",
  "first_party_entrance_time_day_brand_ID", 
  "NO_type_1_deactivated_in_product_space", "NO_type_2_deactivated_in_product_space", 
  "NO_type_1_activated_in_product_space", "NO_type_2_activated_in_product_space",
  "total_sku_type_1_in_product_space", 
  "NO_type_1_deactivated_in_product_spacebrand_ID", "NO_type_2_deactivated_in_product_spacebrand_ID", 
  "NO_type_1_activated_in_product_spacebrand_ID", "NO_type_2_activated_in_product_spacebrand_ID",
  "total_sku_type_1_in_product_spacebrand_ID", "total_sku_type_2_in_product_spacebrand_ID", 
  "total_sku_in_product_spacebrand_ID", "non_NA_input_in_product_space", "type","total_sku_type_2_in_product_space","total_sku_in_product_space"
)

# Helper for safe value selection
safe_first <- function(x) {
  if (all(is.na(x))) NA else first(na.omit(x))
}

# Static info per SKU
sku_static_info <- sku_order %>%
  group_by(sku_ID) %>%
  summarise(across(all_of(cols_to_impute), safe_first), .groups = "drop")

# Create grid for all SKU x day combinations
grid <- expand.grid(sku_ID = unique(sku_order$sku_ID), day_of_month = 1:31)

# Construct panel and fill static info
balanced_panel <- grid %>%
  left_join(sku_static_info, by = "sku_ID")

# Add `after_enter` indicator
balanced_panel <- balanced_panel %>%
  mutate(after_enter = ifelse(is.na(first_party_entrance_time_day), 0,
                              ifelse(day_of_month >= as.numeric(first_party_entrance_time_day), 1, 0)))


# Add average price and first order count per SKU-day
price_andorder_summary <- sku_order %>%
  group_by(sku_ID, day_of_month) %>%
  summarise(
    imputed_ave_final_unit_price = mean(final_unit_price, na.rm = TRUE),
    NO_sales_within_day = first(NO_sales_within_day),
    .groups = "drop"
  )

# Join and fill price + sales, handle NAs
balanced_panel <- balanced_panel %>%
  left_join(price_andorder_summary, by = c("sku_ID", "day_of_month")) %>%
  arrange(sku_ID, day_of_month) %>%
  group_by(sku_ID) %>%
  mutate(
    imputed_ave_final_unit_price = zoo::na.locf(imputed_ave_final_unit_price, na.rm = FALSE),
    imputed_ave_final_unit_price = zoo::na.locf(imputed_ave_final_unit_price, fromLast = TRUE, na.rm = FALSE),
    NO_sales_within_day = replace_na(NO_sales_within_day, 0)  # Replace NA sales with 0
  ) %>%
  ungroup()


# Save final balanced panel
write_csv(balanced_panel, glue("{data_path}/balanced_panel.csv"))
