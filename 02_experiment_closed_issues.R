library(tidyverse)
library(dotenv)
library(readxl)
library(lubridate)
library(zoo)
library(gsynth)
library(augsynth)
library(panelView)

# load functions from another code module in r
source("src/setup.R")
#remove scientific notation
options(scipen = 999)

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

issues <- as_tibble(tbl(con, Id(schema = "public", table = "issues")))

initial_balances <- issues %>%
  filter(created_at < as.Date("2020-01-01")) %>%
  filter(is.na(closed_at) | closed_at >= as.Date("2020-01-01")) %>%
  group_by(repo_sha_id) %>%
  summarise(initial_count = n(), .groups = "drop")

issue_status_over_time <- issues %>%
  pivot_longer(
    cols = c(created_at, closed_at),
    names_to = "event_type",
    values_to = "event_time"
  ) %>%
  filter(!is.na(event_time)) %>%
  mutate(
    floor_month = as.Date(floor_date(event_time, "quarter")),
    change = if_else(event_type == "created_at", 1, -1)
  ) %>%
  filter(floor_month >= as.Date("2020-01-01") & floor_month <= as.Date("2025-12-01")) %>%
  group_by(repo_sha_id, floor_month) %>%
  summarise(
    monthly_net_change = sum(change),
    n_issues_closed = sum(event_type == "closed_at"),
    .groups = "drop"
  ) 

# Create the master grid of all possible combinations
all_months <- seq(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "quarter")
all_repos <- unique(issue_status_over_time$repo_sha_id)

issue_status_over_time <- issue_status_over_time %>%
  # Use complete to force the full grid for EVERY repo
  complete(repo_sha_id = all_repos, floor_month = all_months, 
           fill = list(monthly_net_change = 0, 
                       n_issues_closed = 0)) %>%
  left_join(initial_balances, by = "repo_sha_id") %>%
  mutate(initial_count = replace_na(initial_count, 0)) %>%
  arrange(repo_sha_id, floor_month) %>%
  group_by(repo_sha_id) %>%
  # Cumulative sum + the starting balance
  mutate(open_issues_count = initial_count + cumsum(monthly_net_change)) %>%
  ungroup()

issue_status_over_time <- issue_status_over_time %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  group_by(repo_sha_id) %>%
  mutate(
    log_n_issues_closed = log(n_issues_closed + 1),  # Add 1 to avoid log(0)
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


ggplot(issue_status_over_time, aes(x = as.Date(floor_month), 
                                      y = log_n_issues_closed,
                                      colour = as.factor(treated))) +
  geom_line(stat = "summary") +
  labs(title = "N of Closed Issues by Treatment Group",
       x = "Month",
       y = "log N") +
  #hide colour legend
  guides(color = "none") +
  theme_minimal()


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


panelview(log_n_issues_closed ~ period_treated + oc_funding + open_issues_count,
          data = issue_status_over_time,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE)

panelview(log_n_issues_closed ~ period_treated + oc_funding + open_issues_count,
          data = issue_status_over_time,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE, 
          type = "outcome")

issue_status_over_time$repo_sha_id <- as.factor(issue_status_over_time$repo_sha_id)
issue_status_over_time$time_period <- as.numeric(issue_status_over_time$time_period)


out <- gsynth(
  formula = log_n_issues_closed ~ period_treated + oc_funding  + open_issues_count,  
  data = issue_status_over_time,
  index = c("repo_sha_id", "time_period"), 
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

syn <- augsynth(log_n_issues_closed ~ period_treated | oc_funding  + open_issues_count ,
                unit = repo_sha_id,
                time = time_period,
                data = issue_status_over_time)

summary(syn)
