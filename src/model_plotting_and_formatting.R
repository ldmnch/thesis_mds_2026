library(janitor)
library(ggplot2)
library(writexl)

# get current datetime

date <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

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
# ── Helpers ───────────────────────────────────────────────────────────────────

make_dir <- function(subfolder) {
  path <- paste0("data/final/", subfolder)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  return(path)
}

save_xlsx <- function(sheets, experiment_name, suffix, subfolder) {
  dir_path  <- make_dir(subfolder)
  xlsx_path <- paste0(dir_path, "/", experiment_name, "_", suffix, ".xlsx")
  write_xlsx(x = sheets, path = xlsx_path)
  message("Saved: ", xlsx_path)
  return(invisible(xlsx_path))
}

# ── GSynth ────────────────────────────────────────────────────────────────────

extract_att_time_gsynth <- function(model) {
  att_est <- model$est.att
  data.frame(
    time        = rownames(att_est),
    estimate    = att_est[, "ATT"],
    std_error   = att_est[, "S.E."],
    p_value     = att_est[, "p.value"],
    lower_bound = att_est[, "CI.lower"],
    upper_bound = att_est[, "CI.upper"]
  )
}

extract_att_avg_gsynth <- function(model) {
  att_avg <- model$est.avg
  data.frame(
    term        = "ATT.avg",
    estimate    = att_avg["ATT.avg", "Estimate"],
    std_error   = att_avg["ATT.avg", "S.E."],
    p_value     = att_avg["ATT.avg", "p.value"],
    lower_bound = att_avg["ATT.avg", "CI.lower"],
    upper_bound = att_avg["ATT.avg", "CI.upper"]
  )
}

extract_effects_gsynth <- function(model, experiment_name, subfolder = date) {
  sheets <- list(
    att_over_time = extract_att_time_gsynth(model),
    att_avg       = extract_att_avg_gsynth(model)
  )
  save_xlsx(sheets, experiment_name, "gsynth", subfolder)
  return(sheets)
}

# ── AugSynth ──────────────────────────────────────────────────────────────────

extract_att_time_augsynth <- function(model) {
  summary(model)$att %>%
    filter(Level == "Average", !is.na(Time)) %>%
    clean_names() %>%
    add_p_values()
}

extract_l2_imbalance_multisynth <- function(model) {
  data.frame(
    metric = c(
      "Global L2 Imbalance (Raw)",
      "Global L2 Imbalance (Scaled)",
      "Global L2 % Improvement",
      "Individual L2 Imbalance (Raw)",
      "Individual L2 Imbalance (Scaled)",
      "Individual L2 % Improvement"
    ),
    value = c(
      model$global_l2,
      model$scaled_global_l2,
      (1 - model$scaled_global_l2) * 100,
      model$ind_l2,
      model$scaled_ind_l2,
      (1 - model$scaled_ind_l2) * 100
    )
  )
}
extract_effects_augsynth <- function(model, experiment_name, subfolder = date) {
  sheets <- list(
    att_over_time = extract_att_time_augsynth(model),
    l2_imbalance  = extract_l2_imbalance_multisynth(model)
  )
  save_xlsx(sheets, experiment_name, "augsynth", subfolder)
  
  # Keep CSV for backwards compatibility
  write_csv(
    sheets$att_over_time,
    paste0(make_dir(subfolder), "/", experiment_name, "_augsynth.csv")
  )
  
  return(sheets)
}

define_plot_title <- function(file_name) {
  if (str_detect(file_name, "02_closed_issues") & str_detect(file_name, "_gsynth")) {
    return("Closed Issues - GSynth")
  } else if (str_detect(file_name, "02_closed_issues") & str_detect(file_name, "augsynth")) {
    return("Closed Issues - AugSynth")
  } else if (str_detect(file_name, "03_releases") & str_detect(file_name, "_gsynth")) {
    return("Releases - GSynth")
  } else if (str_detect(file_name, "03_releases") & str_detect(file_name, "augsynth")) {
    return("Releases - AugSynth") 
  } else if (str_detect(file_name, "04_merged_crs") & str_detect(file_name, "_gsynth")) {
    return("Merged CRs - GSynth")
  } else if (str_detect(file_name, "04_merged_crs") & str_detect(file_name, "augsynth")) {
    return("Merged CRs - AugSynth")
  }
  else if (str_detect(file_name, "05_merged_crs") & str_detect(file_name, "_gsynth")) {
    return("New CRs - GSynth")
  } else if (str_detect(file_name, "05_merged_crs") & str_detect(file_name, "augsynth")) {
    return("New CRs - AugSynth")
  } else if (str_detect(file_name, "06_commiters") & str_detect(file_name, "_gsynth")) {
    return("Committers - GSynth")
  } else if (str_detect(file_name, "06_commiters") & str_detect(file_name, "augsynth")) {
    return("Committers - AugSynth")
  } else if (str_detect(file_name, "07_commits") & str_detect(file_name, "_gsynth")) {
    return("Commits - GSynth")
  } else if (str_detect(file_name, "07_commits") & str_detect(file_name, "augsynth")) {
    return("Commits - AugSynth")
  } else if (str_detect(file_name, "08_new_issues") & str_detect(file_name, "_gsynth")) {
    return("New Issues - GSynth")
  } else if (str_detect(file_name, "08_new_issues") & str_detect(file_name, "augsynth")) {
    return("New Issues - AugSynth")
   }
    else {
    return("Unknown Experiment")
  }
}

plot_coefficients <- function(model_results_path, 
                              title = define_plot_title(model_results_path)) {
  
  if (str_detect(model_results_path, "augsynth")) {
    
    model <- read_csv(model_results_path) %>%
      clean_names()
    
    model_filtered_post <- model %>%
      filter(time > 0)
    
    avg_att <- model_filtered_post %>%
      summarize(avg_att = mean(estimate, na.rm = TRUE)) %>%
      pull(avg_att)
    
    avg_pval <- model_filtered_post %>%
      summarize(avg_p_val = mean(p_value, na.rm = TRUE)) %>%
      pull(avg_p_val)
      
  } else {
    
    model <- read_xlsx(model_results_path, sheet = "att_over_time") %>%
      clean_names() %>%
      mutate(time = as.numeric(time))
    
    model_avg <- read_xlsx(model_results_path, sheet = "att_avg") %>%
      clean_names()
    
    avg_att <- model_avg %>%
      pull(estimate)
    
    avg_pval <- model_avg %>%
      pull(p_value)
    
    }
  

  epigraph <- sprintf("Avg. ATT: %.4f  |  Avg. p-value: %.4f", avg_att, avg_pval)
  
  ggplot(model, aes(x = time, y = estimate, group = 1)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
    labs(
      title = title,
      subtitle = "Pre-treatment period: Time < 0 | Post-treatment period: Time ≥ 0",
      caption = epigraph,
      x = "Time Since Treatment",
      y = "ATT Estimate"
    ) +
    theme_minimal() +
    theme(plot.caption = element_text(hjust = 0.5, face = "italic", size = 10))
  
}
