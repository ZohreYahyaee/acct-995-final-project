# 01_clean_sku_data.R
# Purpose: Load and clean SKU-level data, create product space features, and save cleaned output

# ──────────────────────────────────────────────────────────────
# Load cleaned project environment
source("00_setup.R")
#calling libraries 
library(glue)
library(lubridate)
library(tidyverse)
# ──────────────────────────────────────────────────────────────
# Load the Raw Data
# ──────────────────────────────────────────────────────────────
#data_path <- Sys.getenv("DATA_PATH")
print(data_path)  # Check the path prints as expected

# Try reading the file again
sku_raw <- read_csv(glue("{data_path}/JD_sku_data.csv"))



# ──────────────────────────────────────────────────────────────
# Data Cleaning and Feature Engineering
# ──────────────────────────────────────────────────────────────

# Step 1: Replace "-" with NA and convert attributes to integers
sku_clean <- sku_raw %>%
  mutate(across(where(is.character), ~ ifelse(.x == "-", NA, .x))) %>%
  mutate(
    attribute1 = as.integer(attribute1),
    attribute2 = as.integer(attribute2)
  )

# Step 2: Create product space ID
sku_clean <- sku_clean %>%
  mutate(product_space = paste(brand_ID, attribute1, attribute2, sep = "_"))

# Step 3: Convert date columns to numeric POSIX format
sku_clean <- sku_clean %>%
  mutate(
    activate_date = as.numeric(as.POSIXct(activate_date, format = "%Y-%m-%d", tz = "UTC")),
    deactivate_date = as.numeric(as.POSIXct(deactivate_date, format = "%Y-%m-%d", tz = "UTC"))
  )

# Step 4: Classify product spaces by type composition
product_space_summary <- sku_clean %>%
  group_by(product_space) %>%
  summarize(type_unique = paste(unique(type), collapse = ",")) %>%
  mutate(product_space_type_classification = case_when(
    type_unique == "1" ~ "Only Type 1",
    type_unique == "2" ~ "Only Type 2",
    type_unique %in% c("1,2", "2,1") ~ "Combination"
  ))

# Merge product space type classification back
sku_clean <- sku_clean %>%
  left_join(product_space_summary %>% select(product_space, product_space_type_classification), by = "product_space")

# Step 5: Feature engineering
sku_clean <- sku_clean %>%
  group_by(product_space) %>%
  mutate(enter_typ_one = ifelse(any(type == 1 & !is.na(activate_date)), 1, 0)) %>%
  ungroup()

sku_clean <- sku_clean %>%
  group_by(brand_ID) %>%
  mutate(enter_typ_one_within_brand = ifelse(any(type == 1 & !is.na(activate_date)), 1, 0)) %>%
  ungroup()

sku_clean <- sku_clean %>%
  group_by(product_space) %>%
  mutate(n_sku_within_product_space = n_distinct(sku_ID)) %>%
  ungroup()

sku_clean <- sku_clean %>%
  group_by(brand_ID) %>%
  mutate(n_sku_within_brand_ID = n_distinct(sku_ID)) %>%
  ungroup()

# Step 6: Compute first-party entry date per product space and brand
sku_clean <- sku_clean %>%
  group_by(product_space) %>%
  mutate(first_entry_date_firstPArty = case_when(
    sum(enter_typ_one == 1 & !is.na(activate_date)) > 1 ~ 
      ifelse(all(is.na(activate_date[enter_typ_one == 1])), NA_real_,
             min(activate_date[enter_typ_one == 1 & !is.na(activate_date)], na.rm = TRUE)),
    sum(enter_typ_one == 1 & !is.na(activate_date)) == 1 ~ 
      activate_date[enter_typ_one == 1 & !is.na(activate_date)][1],
    TRUE ~ NA_real_
  )) %>%
  ungroup()

sku_clean <- sku_clean %>%
  group_by(brand_ID) %>%
  mutate(first_entry_date_firstPArtybrand_ID = case_when(
    sum(enter_typ_one_within_brand == 1 & !is.na(activate_date)) > 1 ~ 
      ifelse(all(is.na(activate_date[enter_typ_one_within_brand == 1])), NA_real_,
             min(activate_date[enter_typ_one_within_brand == 1 & !is.na(activate_date)], na.rm = TRUE)),
    sum(enter_typ_one_within_brand == 1 & !is.na(activate_date)) == 1 ~ 
      activate_date[enter_typ_one_within_brand == 1 & !is.na(activate_date)][1],
    TRUE ~ NA_real_
  )) %>%
  ungroup()

# Step 7: Extract day of entrance
sku_clean <- sku_clean %>%
  mutate(
    first_party_entrance_time_day = day(as.POSIXct(first_entry_date_firstPArty, origin = "1970-01-01", tz = "UTC")),
    first_party_entrance_time_day_brand_ID = day(as.POSIXct(first_entry_date_firstPArtybrand_ID, origin = "1970-01-01", tz = "UTC"))
  )

# Step 8: Aggregate SKU counts by type and status within product space and brand
product_space_counts <- sku_clean %>%
  group_by(product_space) %>%
  summarize(
    NO_type_1_deactivated_in_product_space = n_distinct(sku_ID[type == 1 & !is.na(deactivate_date)]),
    NO_type_2_deactivated_in_product_space = n_distinct(sku_ID[type == 2 & !is.na(deactivate_date)]),
    NO_type_1_activated_in_product_space = n_distinct(sku_ID[type == 1 & !is.na(activate_date)]),
    NO_type_2_activated_in_product_space = n_distinct(sku_ID[type == 2 & !is.na(activate_date)]),
    total_sku_type_1_in_product_space = n_distinct(sku_ID[type == 1]),
    total_sku_type_2_in_product_space = n_distinct(sku_ID[type == 2]),
    total_sku_in_product_space = n_distinct(sku_ID)
  )

brand_counts <- sku_clean %>%
  group_by(brand_ID) %>%
  summarize(
    NO_type_1_deactivated_in_product_spacebrand_ID = n_distinct(sku_ID[type == 1 & !is.na(deactivate_date)]),
    NO_type_2_deactivated_in_product_spacebrand_ID = n_distinct(sku_ID[type == 2 & !is.na(deactivate_date)]),
    NO_type_1_activated_in_product_spacebrand_ID = n_distinct(sku_ID[type == 1 & !is.na(activate_date)]),
    NO_type_2_activated_in_product_spacebrand_ID = n_distinct(sku_ID[type == 2 & !is.na(activate_date)]),
    total_sku_type_1_in_product_spacebrand_ID = n_distinct(sku_ID[type == 1]),
    total_sku_type_2_in_product_spacebrand_ID = n_distinct(sku_ID[type == 2]),
    total_sku_in_product_spacebrand_ID = n_distinct(sku_ID)
  )

# Join aggregates back to main data
sku_clean <- sku_clean %>%
  left_join(product_space_counts, by = "product_space") %>%
  left_join(brand_counts, by = "brand_ID")

# Step 9: NA status and attribute flags
sku_clean <- sku_clean %>%
  mutate(
    non_NA_input_in_product_space = ifelse(!is.na(attribute1) & !is.na(attribute2) & !is.na(brand_ID), 1, 0),
    with_any_in_only_one_of_attribute = case_when(
      xor(is.na(attribute1), is.na(attribute2)) ~ 1,
      is.na(attribute1) & is.na(attribute2) ~ 0,
      TRUE ~ 0
    )
  )

# ──────────────────────────────────────────────────────────────
# Export Cleaned Data
# ──────────────────────────────────────────────────────────────
write_csv(sku_clean, glue("{data_path}/sku_after_cleaning.csv"))
