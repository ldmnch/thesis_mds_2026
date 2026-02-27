options(scipen = 999)

experiment_name <- "04_merged_crs"

merged_crs_frequency <- build_experiment_panel(
  raw_df     = load_table(con, table = "prs"),
  repos      = load_repos(),
  date_col   = "merged_at",
  target_col = "n_merged_crs"
)

ggplot(merged_crs_frequency, aes(x = floor_month, y = log_n_merged_crs, color = as.factor(treated))) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Number of Merged PRs Over Time by Treatment Status",
       x = "Time (Quarterly)", y = "Average Number of Merged PRs") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  theme(legend.title = element_blank())


out_gsynth <- train_gsynth_model(
  data = merged_crs_frequency, 
  target = "log_n_merged_crs",
  index = c("repo_sha_id", "time_period"))

merged_crs_frequency_clean <- merged_crs_frequency %>%
  drop_na(size, stargazers_count)

syn <- train_augsynth_model(
  data = merged_crs_frequency_clean, 
  target = "log_n_merged_crs",
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
