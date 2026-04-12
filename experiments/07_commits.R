experiment_name <- "07_commits"

#remove scientific notation
options(scipen = 999)

commits_n <- build_experiment_panel(
  raw_df     = load_table(con, table = "commits"),
  repos      = load_repos(),
  date_col   = "date",
  target_col = "total_commits"
)

paralell_trends_plot(commits_n, "log_total_commits")


out_gsynth <- train_gsynth_model(
  data = commits_n, 
  target = "log_total_commits",
  index = c("repo_sha_id", "time_period"))


syn <- train_augsynth_model(
  data = commits_n, 
  target = "log_total_commits",
  covariates = FALSE,
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
