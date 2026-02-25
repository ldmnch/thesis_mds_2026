library(ggplot2)
library(janitor)

add_p_values <- function(data) {
  # 1. Calculate the Z-score
  z_score <- data$estimate / data$std_error
  
  # 2. Calculate the two-sided p-value
  data$p_value <- 2 * (1 - pnorm(abs(z_score)))
  
  # 3. Optional: Add "significance stars" for easy reading
  data$sig <- cut(data$p_value, 
                  breaks = c(-Inf, 0.01, 0.05, 0.1, Inf), 
                  labels = c("***", "**", "*", ""))
  
  return(data)
}

extract_effects_augsynth <- function(model) {
  # Extract the summary table
  summary_table <- summary(model)$att
  
  avg_results <- summary_table %>%
    filter(Level == "Average", !is.na(Time)) %>%
    clean_names()
  
  avg_results <- add_p_values(avg_results)
  
  return(avg_results)
}

extract_effects_gsynth <- function(model) {
  # Extract the ATT estimates and confidence intervals
  att_est <- model$est.att
  att_avg <- model$est.avg
  
  # Create a data frame for ATT over time
  att_df <- data.frame(
    time = rownames(att_est),
    estimate = att_est[, "ATT"],
    std_error = att_est[, "S.E."],
    lower_bound = att_est[, "CI.lower"],
    upper_bound = att_est[, "CI.upper"]
  )
  
  att_df <- add_p_values(att_df)
  
  # Create a data frame for average ATT
  att_avg_df <- data.frame(
    term = "ATT.avg",
    estimate = att_avg["ATT.avg", "Estimate"],
    std_error = att_avg["ATT.avg", "S.E."],
    lower_bound = att_avg["ATT.avg", "CI.lower"],
    upper_bound = att_avg["ATT.avg", "CI.upper"]
  )
  
  return(list(att_over_time = att_df, att_avg = att_avg_df))
}

plot_coefficients_augsynth <- function(model, title = "ATT Estimates Over Time") {
  
  ggplot(model, aes(x = time, y = estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(title = title,
         subtitle = "Pre-treatment period: Time < 0 | Post-treatment period: Time ≥ 0",
         x = "Time Since Treatment",
         y = "ATT Estimate") +
    theme_minimal()
  
  

}

plot_coefficients_gsynth <- function(model, title = "ATT Estimates Over Time") {
  
  ggplot(model$att_over_time, aes(x = as.integer(time), y = estimate, group = 1)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(title = "Average Treatment Effect Over Time",
         subtitle = "Pre-treatment period: Time < 0 | Post-treatment period: Time ≥ 0",
         x = "Time Since Treatment",
         y = "ATT Estimate") +
    theme_minimal()
  
}
