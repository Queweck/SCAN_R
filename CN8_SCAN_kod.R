################################################################################
# SCAN Methodology Data Pipeline (EU JRC Methodology)
# Script for fetching Eurostat COMEXT & PRODCOM trade data and calculating 
# SCAN supply chain risk indicators.
################################################################################

library(tidyverse)
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)

# Base Eurostat API URL
BASE_URL <- "https://ec.europa.eu/eurostat/api/comext/dissemination/statistics/1.0/data/ds-045409"

# List of EU-27 member states and regional aggregates to exclude for extra-EU analysis
EU_EXTRA_EXCLUDE <- c(
  "WORLD", "TOTAL", "EXT_EU27_2020", "INT_EU27_2020", "EU27_2020",
  "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "EL", "ES", "FI", 
  "FR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", 
  "PT", "RO", "SE", "SI", "SK"
)

# ------------------------------------------------------------------------------
# 1. DATA LOADING AND PREPARATION
# ------------------------------------------------------------------------------

# Load product mapping and classification crosswalks
product_mapping <- read_excel("JRC kody.xlsx")
prodcom_mapping <- read_excel("PRODCOM.xlsx")

# Update product codes for historical consistency (post-2021 CN code revisions)
product_mapping <- product_mapping %>%
  mutate(
    jrc_code = as.numeric(JRC_kod),
    jrc_code_2021 = case_match(
      jrc_code,
      81129940 ~ 81129920,
      81129950 ~ 81129930,
      85414100 ~ 85414010,
      85414900 ~ 85414090,
      85415100 ~ 85415000,
      85415900 ~ 85415000,
      .default = jrc_code
    )
  )

# Extract code vectors
codes_current <- product_mapping$jrc_code
codes_2021    <- product_mapping$jrc_code_2021

# ------------------------------------------------------------------------------
# 2. API FETCHING FUNCTIONS
# ------------------------------------------------------------------------------

#' Fetch annual trade data from Eurostat COMEXT API
#'
#' @param product_codes Vector of product codes (CN8)
#' @param target_year Target year string (e.g., "2025")
#' @return A tibble with columns: product, partner, flow, value_eur
fetch_comext_annual <- function(product_codes, target_year) {
  fetch_single <- function(code) {
    params <- list(
      freq = "A",
      reporter = "EU27_2020",
      indicators = "VALUE_IN_EUROS",
      time = target_year,
      product = as.character(code),
      lang = "en"
    )
    
    res <- GET(BASE_URL, query = params)
    
    if (status_code(res) != 200) {
      warning(sprintf("Failed to fetch annual data for code %s (Status: %d)", code, status_code(res)))
      return(NULL)
    }
    
    raw_text <- content(res, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(raw_text)
    
    if (is.null(json_data$value) || length(json_data$value) == 0) {
      return(NULL)
    }
    
    partners <- names(json_data$dimension$partner$category$index)
    flows    <- names(json_data$dimension$flow$category$index)
    val_vec  <- unlist(json_data$value)
    
    df <- expand.grid(
      flow = flows,
      partner = partners,
      stringsAsFactors = FALSE
    )
    df$product <- as.numeric(code)
    
    idx_keys <- as.character(0:(nrow(df) - 1))
    df$value_eur <- as.numeric(val_vec[idx_keys])
    
    return(df[, c("product", "partner", "flow", "value_eur")])
  }
  
  map_dfr(product_codes, fetch_single)
}

#' Fetch monthly trade data from Eurostat COMEXT API
#'
#' @param product_codes Vector of product codes
#' @param start_period Start period string (e.g., "2021-09")
#' @param end_period End period string (e.g., "2021-12")
#' @return A tibble with monthly trade indicators
fetch_comext_monthly <- function(product_codes, start_period, end_period) {
  fetch_single <- function(code) {
    clean_code <- as.character(code)
    if (nchar(clean_code) == 6) {
      clean_code <- paste0(clean_code, "00")
    }
    
    params <- list(
      freq = "M",
      reporter = "EU27_2020",
      product = clean_code,
      sinceTimePeriod = start_period,
      untilTimePeriod = end_period,
      lang = "en"
    )
    
    res <- GET(BASE_URL, query = params)
    if (status_code(res) != 200) {
      message(sprintf("HTTP Error %d for monthly query on code: %s", status_code(res), clean_code))
      return(NULL)
    }
    
    raw_text <- content(res, as = "text", encoding = "UTF-8")
    json_data <- fromJSON(raw_text)
    
    if (is.null(json_data$value) || length(json_data$value) == 0) {
      message(sprintf("No monthly data found for code: %s", clean_code))
      return(NULL)
    }
    
    dim_names <- names(json_data$dimension)
    dim_list  <- lapply(dim_names, function(d) names(json_data$dimension[[d]]$category$index))
    names(dim_list) <- dim_names
    
    df <- do.call(expand.grid, c(rev(dim_list), stringsAsFactors = FALSE))
    df <- df[, rev(seq_along(dim_list))]
    
    val_vec  <- unlist(json_data$value)
    idx_keys <- as.character(0:(nrow(df) - 1))
    
    df$product   <- clean_code
    df$value_eur <- as.numeric(val_vec[idx_keys])
    
    message(sprintf("Fetched code: %s | Valid observations: %d", clean_code, sum(!is.na(df$value_eur))))
    return(df)
  }
  
  map_dfr(product_codes, fetch_single)
}

# ------------------------------------------------------------------------------
# 3. INDICATOR COMPUTATION FUNCTIONS
# ------------------------------------------------------------------------------

#' Compute SCAN structural indicators for a given annual trade dataset
#'
#' @param trade_data Data frame returned by fetch_comext_annual
#' @param prod_mapping Product code mapping table
#' @param prc_mapping PRC to CN code mapping table
#' @param code_col_name String name of the product code column in prod_mapping
#' @return Data frame of structural indicators per product
compute_structural_indicators <- function(trade_data, prod_mapping, prc_mapping, code_col_name = "jrc_code") {
  
  # Top supplier and concentration indexes (HHI & Max Market Share)
  concentration_metrics <- trade_data %>%
    filter(
      flow == "1", # 1 = Import
      !partner %in% EU_EXTRA_EXCLUDE,
      !is.na(value_eur)
    ) %>%
    group_by(product) %>%
    mutate(
      total_import = sum(value_eur, na.rm = TRUE),
      market_share = value_eur / total_import
    ) %>%
    summarise(
      top_source = partner[which.max(value_eur)],
      max_market_share = max(market_share, na.rm = TRUE) * 100,
      hhi = sum(market_share^2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Total Extra-EU Imports
  extra_eu_imports <- trade_data %>%
    filter(flow == "1", partner == "EXT_EU27_2020") %>%
    group_by(product) %>%
    summarise(extra_eu_imp = sum(value_eur, na.rm = TRUE), .groups = "drop")
  
  # Total EU Exports (Extra-EU + Intra-EU)
  total_exports <- trade_data %>%
    filter(flow == "2", partner %in% c("EXT_EU27_2020", "INT_EU27_2020")) %>%
    group_by(product) %>%
    summarise(total_export = sum(value_eur, na.rm = TRUE), .groups = "drop")
  
  # Ratio Import / Export
  ratio_imp_exp <- extra_eu_imports %>%
    inner_join(total_exports, by = "product") %>%
    mutate(ratio_imp_exp = extra_eu_imp / total_export)
  
  # Exposure Index Calculation
  mapped_codes <- prod_mapping %>%
    select(product = !!sym(code_col_name), PRC_kod, description = opis) %>%
    distinct()
  
  exports_weighted <- total_exports %>%
    inner_join(mapped_codes, by = "product") %>%
    inner_join(prc_mapping, by = "PRC_kod") %>%
    mutate(
      value_eur_prod = suppressWarnings(as.numeric(if_else(value_eur_prod == ":", "0", as.character(value_eur_prod)))),
      value_eur_prod = replace_na(value_eur_prod, 0)
    ) %>%
    group_by(PRC_kod) %>%
    mutate(
      total_exp_prodcom = sum(total_export, na.rm = TRUE),
      w_i = if_else(total_exp_prodcom > 0, total_export / total_exp_prodcom, 0)
    ) %>%
    ungroup() %>%
    mutate(prod_w = value_eur_prod * w_i)
  
  exposure_metrics <- extra_eu_imports %>%
    inner_join(exports_weighted, by = "product") %>%
    mutate(
      exposure = extra_eu_imp / (extra_eu_imp + prod_w) * 100
    ) %>%
    select(product, extra_eu_imp, prod_w, exposure)
  
  # Combine structural indicators
  concentration_metrics %>%
    inner_join(ratio_imp_exp, by = "product") %>%
    inner_join(exposure_metrics, by = "product") %>%
    select(product, top_source, max_market_share, hhi, ratio_imp_exp, exposure)
}

#' Process monthly trade dataset to calculate mean prices and quantities
#'
#' @param monthly_df Data frame from fetch_comext_monthly
#' @return Data frame with mean price per 100kg and volume
compute_monthly_metrics <- function(monthly_df) {
  monthly_df %>%
    filter(indicators %in% c("VALUE_IN_EUROS", "QUANTITY_IN_100KG")) %>%
    group_by(product, indicators) %>%
    summarise(mean_val = mean(value_eur, na.rm = TRUE), .groups = "drop") %>%
    pivot_wider(names_from = indicators, values_from = mean_val) %>%
    mutate(
      product = as.numeric(product),
      price = VALUE_IN_EUROS / QUANTITY_IN_100KG
    )
}

# ------------------------------------------------------------------------------
# 4. MAIN EXECUTION PIPELINE
# ------------------------------------------------------------------------------

cat("1/4 Fetching annual datasets from Eurostat...\n")
annual_data_2025 <- fetch_comext_annual(codes_current, "2025")
annual_data_2021 <- fetch_comext_annual(codes_2021, "2021")

cat("2/4 Computing structural indicators...\n")
structural_2025 <- compute_structural_indicators(
  annual_data_2025, product_mapping, prodcom_mapping, code_col_name = "jrc_code"
)

structural_2021 <- compute_structural_indicators(
  annual_data_2021, product_mapping, prodcom_mapping, code_col_name = "jrc_code_2021"
) %>%
  left_join(
    product_mapping %>% select(jrc_code_2021, jrc_code) %>% distinct(),
    by = c("product" = "jrc_code_2021"),
    relationship = "many-to-many"
  ) %>%
  rename(product_2021 = product, product = jrc_code)

cat("3/4 Fetching and computing high-frequency (monthly) indicators...\n")
monthly_data_2021 <- fetch_comext_monthly(codes_2021, "2021-09", "2021-12")
monthly_data_2025 <- fetch_comext_monthly(codes_current, "2025-09", "2025-12")

monthly_metrics_2021 <- compute_monthly_metrics(monthly_data_2021) %>%
  rename_with(~ paste0(.x, "_21"), -product)

monthly_metrics_2025 <- compute_monthly_metrics(monthly_data_2025) %>%
  rename_with(~ paste0(.x, "_25"), -product)

# Merge monthly datasets and map historical codes
monthly_summary <- monthly_metrics_2025 %>%
  right_join(
    product_mapping %>% select(jrc_code, jrc_code_2021, opis),
    by = c("product" = "jrc_code")
  ) %>%
  inner_join(monthly_metrics_2021, by = c("jrc_code_2021" = "product")) %>%
  mutate(
    price_change = (price_25 - price_21) / price_21 * 100,
    quantity_change = (QUANTITY_IN_100KG_25 - QUANTITY_IN_100KG_21) / QUANTITY_IN_100KG_21 * 100
  )

cat("4/4 Generating final report table...\n")

# Prepare 2025 structural summary table
struct_summary_2025 <- structural_2025 %>%
  inner_join(product_mapping, by = c("product" = "jrc_code")) %>%
  mutate(description = str_replace_all(opis, ";", ".")) %>%
  select(product, description, top_source, max_market_share, hhi, ratio_imp_exp, exposure) %>%
  rename_with(~ paste0(.x, "_25"), -c(product, description))

# Prepare 2021 structural summary table
struct_summary_2021 <- structural_2021 %>%
  select(product, max_market_share, hhi, ratio_imp_exp, exposure) %>%
  rename_with(~ paste0(.x, "_21"), -product)

# Consolidate final dataset
scan_final_table <- struct_summary_2025 %>%
  inner_join(struct_summary_2021, by = "product") %>%
  inner_join(monthly_summary, by = "product") %>%
  mutate(
    conc_change = ((max_market_share_25 - max_market_share_21) / max_market_share_21 + 
                     (hhi_25 - hhi_21) / hhi_21) / 2 * 100,
    subs_change = ((ratio_imp_exp_25 - ratio_imp_exp_21) / ratio_imp_exp_21 + 
                     (exposure_25 - exposure_21) / exposure_21) / 2 * 100
  ) %>%
  select(
    product,
    description,
    top_source_25,
    max_market_share_25,
    hhi_25,
    ratio_imp_exp_25,
    exposure_25,
    conc_change,
    subs_change,
    price_change,
    quantity_change
  )%>%
  # Keep only unique product codes
  distinct(product, .keep_all = TRUE)

# Export final report to CSV
write.csv(scan_final_table, "SCAN_indicators_summary.csv")

# Missing products verification
missing_products <- scan_final_table %>%
  right_join(product_mapping, by = c("product" = "jrc_code")) %>%
  filter(is.na(hhi_25)) %>%
  select(product, description = opis)

cat("Process completed successfully. Exported 'SCAN_indicators_summary.csv'.\n")