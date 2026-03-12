experiment_name <- "03_releases"

releases_frequency <- build_experiment_panel(
  raw_df     = load_table(con, table = "releases"),
  repos      = load_repos(),
  date_col   = "published_at",
  target_col = "releases_n"
)


out_gsynth <- train_gsynth_model(
  data = releases_frequency, 
  target = "log_releases_n",
  index = c("repo_sha_id", "time_period"))

syn <- train_augsynth_model(
  data = releases_frequency, 
  covariates = TRUE,
  target = "log_releases_n",
  unit = repo_sha_id,
  time = time_period
)


gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
