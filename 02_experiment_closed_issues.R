library(tidyverse)
library(dotenv)
library(readxl)
library(lubridate)
library(zoo)

# load functions from another code module in r
source("src/setup.R")
#remove scientific notation
options(scipen = 999)

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

issues <- as_tibble(tbl(con, Id(schema = "public", table = "issues")))


issue_status_over_time <- issues %>%
  pivot_longer(
    cols = c(created_at, closed_at),
    names_to = "event_type",
    values_to = "event_time"
  ) %>%
  filter(!is.na(event_time)) %>%
  mutate(
    floor_month = as.Date(floor_date(event_time, "month")),
    change = if_else(event_type == "created_at", 1, -1)
  ) %>%
  filter(year(floor_month) > 2009 & year(floor_month) < 2026) %>%
  group_by(repo_sha_id, floor_month) %>%
  summarise(
    monthly_net_change = sum(change),
    n_issues_closed = sum(event_type == "closed_at"),
    .groups = "drop"
  ) %>%
  arrange(repo_sha_id, floor_month) %>%
  group_by(repo_sha_id) %>%
  mutate(open_issues_count = cumsum(monthly_net_change)) %>%
  ungroup()

# Complete the data to ensure all repo-month combinations are present
issue_status_over_time <- issue_status_over_time %>%
  group_by(repo_sha_id) %>%
  mutate(first_month_of_repo = min(floor_month)) %>%
  ungroup() %>%
  complete(
    repo_sha_id, 
    floor_month = seq(as.Date("2010-01-01"), as.Date("2025-12-01"), by = "month")
  ) %>%
  arrange(repo_sha_id, floor_month) %>%
  group_by(repo_sha_id) %>%
  fill(first_month_of_repo, .direction = "downup") %>% 
  filter(floor_month >= first_month_of_repo) %>%
  mutate(
    monthly_net_change = replace_na(monthly_net_change, 0),
    n_issues_closed = replace_na(n_issues_closed, 0)
  ) %>%
  fill(open_issues_count, .direction = "down") %>%
  ungroup()

issue_status_over_time <- issue_status_over_time %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  group_by(repo_sha_id) %>%
  mutate(
    time_period = as.numeric(as.factor(floor_month)),
    repo_group_id = as.integer(repo_group_id),
    treated = if_else(repo_group_id < 200, 1L, 0L),
    period_treated = if_else(repo_group_id < 200 & floor_month >= sta_start_date, 1L, 0L),
    oc_funding = if_else(floor_month >= oc_funding_start_at, 1L, 0L)
  ) %>%
  ungroup()


ggplot(issue_status_over_time, aes(x = as.Date(floor_month), 
                                      y = monthly_net_change,
                                      colour = as.factor(treated))) +
  geom_point() +
  facet_wrap(~ treated) +
  labs(title = "Monthly Net Change per Treatment Group",
       x = "Month",
       y = "Change") +
  #hide colour legend
  guides(color = "none") +
  theme_minimal()

# remove outlier unit

#issue_resolution_duration <- issue_resolution_duration %>%
#  group_by(repo_sha_id) %>%
#  mutate(mean_issues = mean(n_issues, na.rm = TRUE)) %>%
#  filter(mean_issues < 300)

#Check for time-varying treatment
issue_status_over_time%>%
  group_by(repo_sha_id, repo_group_id) %>%
  summarise(treatment_changes = n_distinct(period_treated)) 

# Check for sufficient pre-treatment periods
issue_status_over_time %>%
  group_by(treated) %>%
  summarise(min_date = min(floor_month), max_date = max(floor_month))

# check for number of treated and non treated units
issue_status_over_time %>%
  group_by(treated) %>%
  summarise(n_repos = n_distinct(repo_sha_id))


ggplot(issue_status_over_time, aes(x = floor_month, y = n_issues_closed, color = factor(treated))) +
  geom_line(stat = "summary", fun = "mean") +
  #geom_smooth()+
  labs(title = "Number of Issues by Treatment Group",
       x = "Month",
       y = "Average Time to Close (days)",
       color = "Treated") +
  theme_minimal()

library(gsynth)
library(augsynth)
library(panelView)

panelview(n_issues_closed ~ period_treated + oc_funding + open_issues_count,
          data = issue_status_over_time,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE)

panelview(n_issues_closed ~ period_treated + oc_funding + open_issues_count,
          data = issue_status_over_time,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE, 
          type = "outcome")

issue_status_over_time$repo_sha_id <- as.factor(issue_status_over_time$repo_sha_id)
issue_status_over_time$time_period <- as.numeric(issue_status_over_time$time_period)

pre_treatment_check <- issue_status_over_time %>%
  group_by(repo_sha_id) %>%
  summarise(
    is_treated = max(period_treated) == 1,
    first_treatment = ifelse(is_treated, min(floor_month[period_treated == 1]), NA),
    n_pre_periods = ifelse(is_treated, sum(floor_month < first_treatment), NA)
  ) %>%
  filter(is_treated)

# Keep only repos with sufficient pre-treatment periods
valid_repos <- pre_treatment_check %>%
  filter(n_pre_periods >= 12) %>%
  pull(repo_sha_id)

# Filter data
filtered_data <- issue_status_over_time %>%
  filter(repo_sha_id %in% valid_repos | treated == 0)


panelview(n_issues_closed ~ period_treated + oc_funding + open_issues_count,
          data = filtered_data,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE)



out <- gsynth(
  formula = n_issues_closed ~ period_treated + oc_funding, 
  data = filtered_data,
  index = c("repo_sha_id", "floor_month"), 
  force = "two-way", 
  r = c(0, 5),  # Use the CV-selected r
  CV = FALSE,        # Don't run CV again
  se = TRUE,         # Now do bootstrap
  na.rm = TRUE, 
  min.T0 = 12,
  nboots = 100,
  parallel = TRUE 
)

out 
plot(out, type = "counterfactual", main = "Counterfactuals (MC)")
plot(out, main = "Estimated ATT")
plot(out, type = "raw")
