experiment_name <- "08_closed_issues"

#remove scientific notation
options(scipen = 999)

new_issues_count <- build_experiment_panel(
  raw_df     = load_table(con, table = "issues"),
  repos      = load_repos(),
  date_col   = "created_at",
  target_col = "issues_new_n"
)



out_gsynth <- train_gsynth_model(
  data = new_issues_count, 
  target = "log_issues_new_n",
  index = c("repo_sha_id", "time_period"))

new_issues_count_clean <- new_issues_count %>%
  drop_na(size, stargazers_count)

syn <- train_augsynth_model(
  data = new_issues_count_clean, 
  target = "log_issues_new_n",
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
