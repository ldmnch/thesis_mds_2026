experiment_name <- "02_closed_issues"

#remove scientific notation
options(scipen = 999)

closed_issues_count <- build_experiment_panel(
  raw_df     = load_table(con, table = "issues"),
  repos      = load_repos(),
  date_col   = "closed_at",
  target_col = "issues_closed_n"
)

paralell_trends_plot(closed_issues_count, "log_issues_closed_n")

out_gsynth <- train_gsynth_model(
  data = closed_issues_count, 
  target = "log_issues_closed_n",
  index = c("repo_sha_id", "time_period"))

syn <- train_augsynth_model(
  data = closed_issues_count, 
  covariates = TRUE,
  target = "log_issues_closed_n",
  unit = repo_sha_id,
  time = time_period
)
gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
