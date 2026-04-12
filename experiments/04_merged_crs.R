options(scipen = 999)

experiment_name <- "04_merged_crs"

merged_crs_frequency <- build_experiment_panel(
  raw_df     = load_table(con, table = "prs"),
  repos      = load_repos(),
  date_col   = "merged_at",
  target_col = "n_merged_crs"
)

paralell_trends_plot(merged_crs_frequency, "log_n_merged_crs")

out_gsynth <- train_gsynth_model(
  data = merged_crs_frequency, 
  target = "log_n_merged_crs",
  index = c("repo_sha_id", "time_period"))


syn <- train_augsynth_model(
  data = merged_crs_frequency,   
  covariates = FALSE,
  target = "log_n_merged_crs",
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
