experiment_name <- "02_closed_issues"

#remove scientific notation
options(scipen = 999)

closed_issues_count <- build_experiment_panel(
  raw_df     = load_table(con, table = "issues"),
  repos      = load_repos(),
  date_col   = "closed_at",
  target_col = "issues_closed_n"
)


ggplot(closed_issues_count, aes(x = as.Date(floor_month), 
                                      y = log_issues_closed_n,
                                      colour = as.factor(treated))) +
  geom_line(stat = "summary") +
  labs(title = "N of Closed Issues by Treatment Group",
       x = "Month",
       y = "log N") +
  #hide colour legend
  guides(color = "none") +
  theme_minimal()

out_gsynth <- train_gsynth_model(
  data = closed_issues_count, 
  target = "log_issues_closed_n",
  index = c("repo_sha_id", "time_period"))

issue_status_clean <- closed_issues_count %>%
  drop_na(size, stargazers_count)

syn <- train_augsynth_model(
  data = issue_status_clean, 
  target = "log_issues_closed_n",
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
