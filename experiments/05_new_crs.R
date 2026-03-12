options(scipen = 999)

experiment_name <- "05_merged_crs"

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

new_crs_frequency <- build_experiment_panel(
  raw_df     = load_table(con, table = "prs"),
  repos      = load_repos(),
  date_col   = "created_at",
  target_col = "n_new_prs"
)

panelview(log_n_new_prs ~ period_treated + oc_funding,
          data = new_crs_frequency,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE, 
          type = "outcome")


ggplot(new_crs_frequency, aes(x = floor_month, y = log_n_new_prs, color = as.factor(treated))) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Number PRs Over Time by Treatment Status",
       x = "Time (Quarterly)", y = "Average Number of Merged PRs") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  theme(legend.title = element_blank())


out_gsynth <- train_gsynth_model(
  data = new_crs_frequency, 
  target = "log_n_new_prs",
  index = c("repo_sha_id", "time_period"))

syn <- train_augsynth_model(
  data = new_crs_frequency, 
  target = "log_n_new_prs",
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
