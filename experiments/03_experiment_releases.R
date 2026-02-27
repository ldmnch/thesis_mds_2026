experiment_name <- "03_releases"

releases_frequency <- build_experiment_panel(
  raw_df     = load_table(con, table = "releases"),
  repos      = load_repos(),
  date_col   = "published_at",
  target_col = "releases_n"
)


ggplot(releases_frequency, aes(x = floor_month, y = log_releases_n, group = repo_sha_id, color = as.factor(oc_funding))) +
  geom_line() +
  labs(title = "Number of Releases Over Time by OC Funding Status", x = "Time Period (Quarter)", y = "Number of Releases", color = "OC Funding") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


out_gsynth <- train_gsynth_model(
  data = releases_frequency, 
  target = "log_releases_n",
  index = c("repo_sha_id", "time_period"))

releases_frequency_clean <- releases_frequency %>%
  drop_na(size, stargazers_count)

syn <- train_augsynth_model(
  data = releases_frequency_clean, 
  target = "log_releases_n",
  unit = repo_sha_id,
  time = time_period
)


gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
