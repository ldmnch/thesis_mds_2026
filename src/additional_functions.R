library(httr2)
library(tibble)
library(dplyr)
library(purrr)
library(jsonlite)
library(glue)
library(readr)
library(tidyr)  

remove_outliers_iqr <- function(data, col) {
  
  metric_column <- data %>% pull({{col}})

  Q1 <- quantile(metric_column, 0.25, na.rm = TRUE)
  Q3 <- quantile(metric_column, 0.75, na.rm = TRUE)
  IQR_val <- IQR(metric_column, na.rm = TRUE)
  
  lower <- Q1 - 1.5 * IQR_val
  upper <- Q3 + 1.5 * IQR_val
  
  data <- data %>%
    filter({{col}} >= lower & {{col}} <= upper)
  
  return(data)
}

flag_outliers_by_group <- function(data, outcome_col, treatment_col) {
  
  data %>%
    group_by({{ treatment_col }}) %>%
    mutate(
      # Compute IQR bounds within each treatment group
      Q1 = quantile({{ outcome_col }}, 0.25, na.rm = TRUE),
      Q3 = quantile({{ outcome_col }}, 0.75, na.rm = TRUE),
      IQR_val = IQR({{ outcome_col }}, na.rm = TRUE),
      lower_bound = Q1 - 1.5 * IQR_val,
      upper_bound = Q3 + 1.5 * IQR_val,
      
      # Flag if this observation is an outlier
      is_outlier = {{ outcome_col }} < lower_bound | {{ outcome_col }} > upper_bound
    ) %>%
    ungroup()
}

check_date_format <- function(data, 
                              period_column, 
                              col_to_change){
  
  col_class <- class(pull(data, {{period_column}}))[1]
  
  # Convert contract dates to match period_column class
  if (col_class == "Date") {
    col_to_change <- as.Date(col_to_change)
  } else if (col_class == "POSIXct") {
    col_to_change <- as.POSIXct(col_to_change)
  } else {
    stop(paste0("period_column must be either Date or POSIXct, date is", col_class))
  }
  
  return(col_to_change)
  
}
