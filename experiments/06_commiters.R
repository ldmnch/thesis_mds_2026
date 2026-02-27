options(scipen = 999)

experiment_name <- "06_commiters"

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

commits <- as_tibble(tbl(con, Id(schema = "public", table = "commits")))
issues <- as_tibble(tbl(con, Id(schema = "public", table = "issues")))

bot_pattern <- "noreply|\\[bot\\]|bot@|dependabot|renovate|github-actions|codecov"

commit_authors <- commits %>%
  filter(!str_detect(tolower(author), bot_pattern)) %>%
  mutate(floor_month = as.Date(floor_date(date, "quarter"))) %>%
  select(repo_sha_id, floor_month, contributor = author)

issue_authors <- issues %>%
  filter(!str_detect(tolower(user_name), bot_pattern)) %>%
  mutate(floor_month = as.Date(floor_date(created_at, "quarter"))) %>%
  select(repo_sha_id, floor_month, contributor = user_name)

contributors_raw <- bind_rows(commit_authors, issue_authors)

contributors_n <- contributors_raw %>%
  aggregate_quarterly(
    df = .,
    id_col = "repo_sha_id",
    date_col = "floor_month",
    target_col = "n_contributors"
    ) %>% 
  complete_quarterly_grid(
    df = .,
    repos = repos,
    id_col = "repo_sha_id",
    repo_id_col = "sha_id",
    target_col = "n_contributors"
    ) %>%
  remove_ghost_repos("repo_sha_id", "n_contributors") %>%
  enrich_variables("repo_sha_id", "n_contributors","log_n_contributors")



ggplot(contributors_n, aes(x = as.Date(floor_month), y = log_n_contributors, color = as.factor(treated))) +
  geom_line( stat = "summary" ) +
  labs(title = "Average Number of Committers Over Time by Treatment Status",
       x = "Time (Quarterly)", y = "Average Number of Committers") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  theme(legend.title = element_blank())



out_gsynth <- train_gsynth_model(
  data = contributors_n, 
  target = "log_n_contributors",
  index = c("repo_sha_id", "time_period"))

contributors_n_clean <- contributors_n %>%
  drop_na(size, stargazers_count)

syn <- train_augsynth_model(
  data = contributors_n_clean, 
  target = "log_n_contributors",
  unit = repo_sha_id,
  time = time_period
)

gsyn_results <- extract_effects_gsynth(out_gsynth, experiment_name)
augsyn_results <- extract_effects_augsynth(syn, experiment_name)
