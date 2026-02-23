library(tidyverse)
library(dotenv)
library(readxl)
library(lubridate)
library(gsynth)
library(augsynth)
library(panelView)

# load functions from another code module in r
source("src/setup.R")
#remove scientific notation
options(scipen = 999)

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

contributors_raw <- bind_rows(commit_authors, issue_authors) %>%
  filter(floor_month >= as.Date("2010-01-01"), floor_month <= as.Date("2025-12-01")) %>%
  group_by(repo_sha_id, floor_month) %>%
  summarise(n_contributors = n_distinct(contributor), .groups = "drop")


all_quarters <- seq(as.Date("2010-01-01"), as.Date("2025-12-01"), by = "quarter")

contributors <- contributors_raw %>%
  complete(repo_sha_id, floor_month = all_quarters, fill = list(n_contributors = 0)) %>%
  # Then restrict to repos that have at least some activity (avoid ghost repos)
  group_by(repo_sha_id) %>%
  filter(sum(n_contributors) > 0) %>%
  ungroup()

contributors <- contributors %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  group_by(repo_sha_id) %>%
  mutate(
    log_contributors = log1p(n_contributors),
    time_period = as.numeric(as.factor(floor_month)),
    repo_group_id = as.integer(repo_group_id),
    treated = if_else(repo_group_id < 200, 1L, 0L),
    period_treated = if_else(repo_group_id < 200 & floor_month >= sta_start_date, 1L, 0L),
    oc_funding = if_else(
      !is.na(oc_funding_start_at) & floor_month >= oc_funding_start_at, 
      1L, 0L
    )
  ) %>%
  ungroup()  

ggplot(contributors, aes(x = as.Date(floor_month), y = log_contributors, color = as.factor(treated))) +
  geom_line( stat = "summary" ) +
  labs(title = "Average Number of Committers Over Time by Treatment Status",
       x = "Time (Quarterly)", y = "Average Number of Committers") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  theme(legend.title = element_blank())

panelview(log_contributors ~ period_treated + oc_funding,
          data = contributors,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE, 
          type = "outcome")


out <- gsynth(
  formula = log_contributors ~ period_treated + oc_funding,  
  data = contributors,
  index = c("repo_sha_id", "time_period"), 
  force = "two-way", 
  r = c(0,10),         
  CV = TRUE,       
  se = TRUE,          # Enable bootstrapping
  nboots = 100,       
  parallel = TRUE,
  min.T0 = 10,        # Ensure at least 12 pre-treatment periods
  na.rm = TRUE
)

out
plot(out, type = "counterfactual", main = "Counterfactuals (MC)")
plot(out, main = "Estimated ATT")
plot(out, type = "raw")


syn <- augsynth(log_contributors ~ period_treated | oc_funding,
                unit = repo_sha_id,
                time = time_period,
                data = contributors)

summary(syn)

# Extract results
results <- summary(syn)$att

# Extract only the "Average" level results
results_avg <- results %>%
  filter(Level == "Average", !is.na(Time))

# Create the plot
ggplot(results_avg, aes(x = Time, y = Estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_bound, ymax = upper_bound), alpha = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Average Treatment Effect Over Time",
       subtitle = "Pre-treatment period: Time < 0 | Post-treatment period: Time ≥ 0",
       x = "Time Since Treatment",
       y = "ATT Estimate") +
  theme_minimal()

