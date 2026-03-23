library(janitor)
library(ggplot2)
library(writexl)
library(stringr)
library(glue)

# get current datetime

date <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# ── Helpers ───────────────────────────────────────────────────────────────────

make_dir_data <- function(subfolder) {
  path <- paste0("data/final/", subfolder)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  return(path)
}

make_dir_plots <- function(subfolder) {
  path <- paste0("plots/", subfolder)
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  return(path)
}

save_xlsx <- function(sheets, experiment_name, suffix, subfolder) {
  dir_path  <- make_dir_data(subfolder)
  xlsx_path <- paste0(dir_path, "/", experiment_name, "_", suffix, ".xlsx")
  write_xlsx(x = sheets, path = xlsx_path)
  message("Saved: ", xlsx_path)
  return(invisible(xlsx_path))
}

# --- Descriptive ----

paralell_trends_plot <- function(df, outcome_variable, subfolder = date) {

  vlines_df <- df %>%
    filter(period_treated == 1) %>%
    distinct(repo_sha_id, period_treated, repo_group_id, time_period) %>%
    group_by(repo_sha_id) %>%
    # filter for the first time period where treated == 1
    filter(time_period == min(time_period))  %>%
    mutate(time_period_jitter = time_period + runif(n(), -0.2, 0.2))
  
  plot <- ggplot(df, aes(x = time_period, y = .data[[outcome_variable]])) +
      geom_vline(
      data = vlines_df,
      aes(xintercept = time_period_jitter),
      color = "black",
      alpha = 0.15,
      linetype = "dashed") +
    geom_smooth(
      aes(
        group = interaction(repo_group_id, treated),
        color = factor(treated)
      ),
      #se = FALSE,
      alpha = 0.2,
      size = 0.8
    ) +
    
    scale_color_manual(
      name = "Group",
      values = c("0" = "#1b9e77", "1" = "#d95f02"),
      labels = c("Control", "Treated")
    ) +
    theme_minimal() +
    labs(title = define_plot_title(experiment_name, plot_type = "Parallel Trends"),
         x = "Time Period (Quarterly)",
         color = "Treated") 
  
  ggsave(
    filename = paste0(make_dir_plots(paste0(subfolder,"/descriptives")), "/", experiment_name, "_parallel_trends.png"),
    plot = plot,
    width = 8,
    height = 6
  )
  
  return(plot)
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
  
  counterfactuals_plot <- plot(model, type = "counterfactual", 
                               main = define_plot_title(paste0(experiment_name,"_gsynth"), plot_type = "Counterfactual"))
  ggsave(
    filename = paste0(make_dir_plots(paste0(subfolder,"/counterfactuals/")), experiment_name, "_gsynth_counterfactual.png"),
    plot = counterfactuals_plot,
    width = 8,
    height = 6
  )
  
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
    clean_names()}

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
  
  counterfactuals_plot <- plot_counterfactuals_multisynth(model)
  
  ggsave(
    filename = paste0(make_dir_plots(paste0(date,"/counterfactuals/")) , experiment_name, "_augsynth_counterfactual.png"),
    plot = counterfactuals_plot,
    width = 8,
    height = 6
  )
  
  sheets <- list(
    att_over_time = extract_att_time_augsynth(model),
    l2_imbalance  = extract_l2_imbalance_multisynth(model)
  )
  save_xlsx(sheets, experiment_name, "augsynth", subfolder)
  
  return(sheets)
}

define_plot_title <- function(file_name, plot_type = "ATT") {
  
  metrics <- c(
    "02_closed_issues" = "Closed Issues",
    "03_releases"      = "Releases",
    "04_merged_crs"    = "Merged CRs",
    "05_merged_crs"    = "New CRs",
    "06_commiters"     = "Contributors",
    "07_commits"       = "Commits",
    "08_new_issues"    = "New Issues"
  )
  
  metric_match <- metrics[str_detect(file_name, names(metrics))]
  metric_label <- if (length(metric_match) > 0) metric_match[1] else "Unknown"
  
  # 3. Extract Model Name
  model_label <- case_when(
    str_detect(file_name, "_gsynth")  ~ "GSynth",
    str_detect(file_name, "augsynth") ~ "AugSynth",
    TRUE                              ~  file_name
  )
  
  glue("{metric_label} ({plot_type}) - {model_label}")
}

compile_l2_improvement_table <- function(folder_path) {
  
  # Find all files ending in _augsynth.xlsx
  files <- list.files(
    path = folder_path,
    pattern = "_augsynth\\.xlsx$",
    full.names = TRUE
  )
  
  if (length(files) == 0) stop("No _augsynth.xlsx files found in folder: ", folder_path)
  
  results <- lapply(files, function(f) {
    
    # Extract outcome model name from filename
    model_name <- tools::file_path_sans_ext(basename(f))
    model_name <- sub("_augsynth$", "", model_name)
    
    # Read second sheet
    df <- read_excel(f, sheet = 2)
    colnames(df) <- c("metric", "value")
    
    # Extract only the L2 % Improvement rows
    df_filtered <- df %>%
      filter(grepl("L2 % Improvement", metric)) %>%
      mutate(
        level = case_when(
          grepl("^Global",     metric) ~ "Global L2 % Improvement",
          grepl("^Individual", metric) ~ "Individual L2 % Improvement",
          TRUE ~ metric
        ),
        model = model_name,
        value = round(as.numeric(value), 2)
      ) %>%
      select(model, level, value)
    
    df_filtered
  })
  
  # Combine and pivot wide: one row per model, columns = Global / Individual
  combined <- bind_rows(results) %>%
    pivot_wider(names_from = level, values_from = value) %>%
    arrange(model)
  
  combined
}

plot_counterfactuals_multisynth <- function(model){
  
  syn_sum <- summary(model)
  
  # 1. Identify treated units
  is_treated <- model$data$trt != Inf
  
  # 2. Get the full observed series for treated units
  # Combine X (pre-period) and y (post-period) matrices horizontally
  full_y_matrix <- cbind(model$data$X[is_treated, ], model$data$y[is_treated, ])
  
  # 3. Calculate the average across those units for all time points
  avg_treated_y <- colMeans(full_y_matrix, na.rm = TRUE)
  
  # 4. Create the plot_data (Now both should have 24 rows)
  att_estimates <- syn_sum$att %>% 
    filter(Level == "Average" & !is.na(Time)) %>%
    arrange(Time)
  
  
  plot_data <- data.frame(
    Time = att_estimates$Time,
    Estimate = att_estimates$Estimate,
    treated_avg = as.numeric(avg_treated_y)
  ) %>%
    mutate(
      counterfactual_avg = treated_avg - Estimate
    )
  
  # 5. Plot
  ggplot(plot_data, aes(x = Time)) +
    geom_vline(xintercept = 0, color = "lightgrey", size = 1.5) +
    # Black line: Actual data
    geom_line(aes(y = treated_avg, color = "Treated Average"), size = 1) +
    # Blue dashed: Estimated (Y0) for the Treated
    geom_line(aes(y = counterfactual_avg, color = "Estimated Y(0) Average"), 
              linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Treated Average" = "black", 
                                  "Estimated Y(0) Average" = "#4682B4")) +
    theme_minimal() +
    labs(title = "Counterfactuals (multisynth)",
         x = "Time relative to Treatment",
         y = "Outcome",
         color = "") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold", size = 16))


}

plot_coefficients <- function(model_results_path, 
                              title = define_plot_title(model_results_path, plot_type = ATT)) {
  
  if (str_detect(model_results_path, "augsynth")) {
    
    model <- read_xlsx(model_results_path, sheet = "att_over_time") %>%
      clean_names()
    
    model_filtered_post <- model %>%
      filter(time > 0)
    
    avg_att <- model_filtered_post %>%
      summarize(avg_att = mean(estimate, na.rm = TRUE)) %>%
      pull(avg_att)
    
    epigraph <- sprintf("Avg. ATT: %.4f ", avg_att)
    
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
    
    epigraph <- sprintf("Avg. ATT: %.4f  |  Avg. p-value: %.4f", avg_att, avg_pval)
    
    }
  

  
  
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
