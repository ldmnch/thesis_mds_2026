library(tidyverse)
library(dotenv)
library(readxl)
library(lubridate)
library(augsynth)
library(gsynth)
library(panelView)


# load functions from another code module in r
source("src/setup.R")
source("src/model_plotting_and_formatting.R")

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

releases <- as_tibble(tbl(con, Id(schema = "public", table = "releases")))

releases %>%
  left_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  group_by(repo_group_id) %>%
  summarise(n_releases = n()) 

releases_frequency <- releases %>%
  mutate(
    floor_month = as.Date(floor_date(published_at, "month")),
  ) %>%
  filter(year(floor_month) > 2019) %>%
  group_by(repo_sha_id, floor_month) %>%
  summarise(
    n_releases = n(),
    .groups = "drop",
  )

releases_frequency <- releases_frequency %>%
  complete(repo_sha_id, floor_month, fill = list(n_releases = 0)) %>%
  mutate(release = ifelse(n_releases > 0, 1, 0)) %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  mutate(time_period = as.numeric(as.factor(floor_month))) %>%
  filter(private_owned == "non_private")

ggplot(releases_frequency, aes(x = time_period, y = release)) +
  geom_point() +
  facet_wrap(~ repo_group_id,
             scales = "free") +
  labs(x = "Time Period (Months)", y = "Number of Releases") +
  theme_minimal()

releases_frequency <- releases_frequency %>%
  mutate(repo_group_id = as.integer(repo_group_id),
         treated = if_else(repo_group_id < 200, 1L, 0L),
         period_treated = case_when(
           repo_group_id < 200 & floor_month < sta_start_date ~ 0L,
           repo_group_id < 200 & floor_month >= sta_start_date & floor_month <= sta_end_date ~ 1L,
           repo_group_id < 200 & floor_month > sta_end_date ~ 1L,
           TRUE ~ 0L),
         oc_funding = if_else(
           !is.na(oc_funding_start_at) & floor_month >= oc_funding_start_at, 
           1L, 0L
         )
         )

releases_frequency %>%
  group_by(oc_funding) %>%
  summarise(n=n())

ggplot(releases_frequency, aes(x = floor_month, y = release, color = as.factor(treated))) +
  stat_summary(fun = "mean", geom = "line") + 
  facet_wrap(~ treated) +
  labs(title = "Release Probability", y = "Probability of New Release (0 to 1)") +
  theme_minimal()


#Check for time-varying treatment
releases_frequency %>%
  group_by(repo_sha_id, repo_group_id) %>%
  summarise(treatment_changes = n_distinct(period_treated)) 

# Check for sufficient pre-treatment periods
releases_frequency %>%
  group_by(treated) %>%
  summarise(min_date = min(floor_month), max_date = max(floor_month))


panelview(release ~ period_treated + oc_funding,
          data = releases_frequency,
          index = c("repo_sha_id", "time_period"),
          pre.post = TRUE)

releases_frequency$repo_sha_id <- as.factor(releases_frequency$repo_sha_id)
releases_frequency$floor_month <- as.factor(releases_frequency$floor_month)


out <- gsynth(
  formula = release ~ period_treated, 
  data = releases_frequency,
  index = c("repo_sha_id", "time_period"), 
  force = "two-way", 
  CV = TRUE,
  r = c(0, 1),           # Stop at 1 factor to prevent "out of bounds"
  se = TRUE, 
  nboots = 100,
  parallel = TRUE,
  min.T0 = 12            # Keep this to ensure enough pre-treatment data
)

out 
plot(out, type = "counterfactual", main = "Counterfactuals (MC)")
plot(out, main = "Estimated ATT")

summary_effects_gsyn <- extract_effects_gsynth(out)

syn <- multisynth(release ~ period_treated, 
                  unit = repo_sha_id, 
                  time = time_period, 
                  data = releases_frequency)

