options(scipen = 999)

experiment_name <- "05_merged_crs"

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

new_crs_frequency <- build_experiment_panel(
  raw_df     = load_table(con, table = "prs"),
  repos      = load_repos(),
  date_col   = "created_at",
  target_col = "n_new_prs"
)

paralell_trends_plot(new_crs_frequency, "log_n_new_prs")

out_gsynth <- train_gsynth_model(
  data = new_crs_frequency, 
  target = "log_n_new_prs",
  index = c("repo_sha_id", "time_period"))

syn <- train_augsynth_model(
  data = new_crs_frequency, 
  target = "log_n_new_prs",
  covariates = FALSE,
  unit = repo_sha_id,
  time = time_period
)


gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
