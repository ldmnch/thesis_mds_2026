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


repos <- repos %>%
  filter(repo_group_id %in% c(105, 102, 204))

issues <- as_tibble(tbl(con, Id(schema = "public", table = "issues")))

# create time_to_close column. for open issues, use the current date as the closed_at date

# Join first, then aggregate
issue_resolution_duration <- issues %>%
  # count closed/open issues per month
  filter(state == "closed") %>%
  mutate(
    floor_month = as.Date(floor_date(closed_at, "month")),
  ) %>%
  filter(year(floor_month) > 2015 & year(floor_month) < 2026) %>%
  group_by(repo_sha_id, floor_month) %>%
  summarise(
    n_issues = n(),
    .groups = "drop",
  )

# Complete the data to ensure all repo-month combinations are present

issue_resolution_duration <- issue_resolution_duration %>%
  complete(repo_sha_id, floor_month, fill = list(n_issues = 0)) %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  #create time period col grouping by repo_sha_id
  mutate(time_period = as.numeric(as.factor(floor_month)))

# add treatment vars
issue_resolution_duration <- issue_resolution_duration %>%
  mutate(repo_group_id = as.integer(repo_group_id),
         repo_group_id = as.integer(repo_group_id),
         treated = if_else(repo_group_id < 200, 1L, 0L),
         period_treated = case_when(
           repo_group_id < 200 & floor_month >= sta_start_date ~ 1L,
           TRUE ~ 0L
         ),
         oc_funding = case_when(
           floor_month < oc_funding_start_at ~ 0L,
           floor_month >= oc_funding_start_at & floor_month <= oc_funding_start_at ~ 1L,
           floor_month > oc_funding_start_at ~ 1L,
           TRUE ~ 0L)
  ) 


ggplot(issue_resolution_duration, aes(x = as.Date(floor_month), 
                                      y = n_issues,
                                      colour = as.factor(treated))) +
  geom_point() +
  facet_wrap(~ treated) +
  labs(title = "Number of Closed Issues by Treatment Group",
       x = "Month",
       y = "Frequency") +
  #hide colour legend
  guides(color = "none") +
  theme_minimal()

# remove outlier unit

issue_resolution_duration <- issue_resolution_duration %>%
  group_by(repo_sha_id) %>%
  mutate(mean_issues = mean(n_issues, na.rm = TRUE)) %>%
  filter(mean_issues < 300)

#Check for time-varying treatment
issue_resolution_duration %>%
  group_by(repo_sha_id, repo_group_id) %>%
  summarise(treatment_changes = n_distinct(period_treated)) 

# Check for sufficient pre-treatment periods
issue_resolution_duration %>%
  group_by(treated) %>%
  summarise(min_date = min(floor_month), max_date = max(floor_month))

ggplot(issue_resolution_duration, aes(x = floor_month, y = n_issues, color = factor(treated))) +
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

panelview(n_issues ~ period_treated + oc_funding,
          data = issue_resolution_duration,
          index = c("repo_sha_id", "time_period"),
          pre.post = TRUE)

panelview(n_issues ~ period_treated + oc_funding,
          data = issue_resolution_duration,
          index = c("repo_sha_id", "time_period"),
          pre.post = TRUE, 
          type = "outcome")

issue_resolution_duration$repo_sha_id <- as.factor(issue_resolution_duration$repo_sha_id)
issue_resolution_duration$time_period <- as.numeric(issue_resolution_duration$time_period)

out <- gsynth(
  formula = n_issues ~ period_treated + oc_funding, 
  data = issue_resolution_duration,
  index = c("repo_sha_id", "time_period"), 
  force = "two-way", 
  r = c(0, 5),      # Range allows CV to work
  CV = TRUE,       # Turn this off
  se = TRUE, 
  na.rm = TRUE, 
  min.T0 = 12,
  nboots = 100,
  parallel = TRUE 
)

out 
plot(out, type = "counterfactual", main = "Counterfactuals (MC)")
plot(out, main = "Estimated ATT")
plot(out, type = "raw")

