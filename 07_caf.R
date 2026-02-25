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

compute_factor <- function(counts, threshold = 0.50) {
  counts_sorted <- sort(counts, decreasing = TRUE)
  total         <- sum(counts_sorted)
  cumulative    <- cumsum(counts_sorted)
  which.max(cumulative >= threshold * total)
}

commits <- commits %>%
  mutate(floor_quarter = floor_date(date, "quarter")) %>%
  filter(floor_quarter >= as.Date("2010-01-01"), floor_quarter <= as.Date("2025-12-31"))

commit_counts <- commits %>%
  group_by(repo_sha_id, floor_quarter, author) %>%
  summarise(n_commits = n(), .groups = "drop") 

# remove commits to free up memory, we only need the counts now
rm(commits)

threshold <- 0.50

# --- 4. Contributor Absence Factor per quarter -----------------
caf <- commit_counts %>%
  group_by(repo_sha_id, floor_quarter) %>%
  summarise(
    total_commits              = sum(n_commits),
    n_contributors             = n_distinct(author),
    contributor_absence_factor = compute_factor(n_commits, threshold),
    .groups = "drop"
  ) 


all_months <- seq(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "quarter")
all_repos <- unique(caf$repo_sha_id)

caf <- caf %>%
  # Use complete to force the full grid for EVERY repo
  complete(repo_sha_id = all_repos, floor_quarter = all_months, 
           fill = list(contributor_absence_factor = NA_integer_)) %>%
  mutate(
    time_period = as.numeric(as.factor(floor_quarter))
  )


caf <- caf %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  group_by(repo_sha_id) %>%
  mutate(
    #log_contributors = log1p(n_contributors),
    repo_group_id = as.integer(repo_group_id),
    treated = if_else(repo_group_id < 200, 1L, 0L),
    period_treated = if_else(repo_group_id < 200 & floor_quarter >= sta_start_date, 1L, 0L),
    oc_funding = if_else(
      !is.na(oc_funding_start_at) & floor_quarter >= oc_funding_start_at, 
      1L, 0L
    )
  ) %>%
  ungroup()  


ggplot(caf, aes(x = time_period, y = contributor_absence_factor, color = as.factor(treated))) +
  geom_point() +
  facet_wrap(~ repo_group_id) +
  labs(title = "Contributor Absence Factor Over Time by Treatment Group",
       x = "Quarter",
       y = "Contributor Absence Factor",
       color = "Treatment Group")  +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  theme(legend.title = element_blank())


panelview(contributor_absence_factor ~ period_treated + oc_funding,
          data = caf,
          index = c("repo_sha_id", "time_period"),
          pre.post = TRUE)

panelview(contributor_absence_factor ~ period_treated + oc_funding,
          data = caf,
          index = c("repo_sha_id", "time_period"),
          pre.post = TRUE, 
          type = "outcome")

# filter repositories 

out <- gsynth(
  formula = contributor_absence_factor ~ period_treated + oc_funding,  
  data = caf,
  index = c("repo_sha_id", "time_period"), 
  force = "two-way", 
  r = c(0,10),         
  CV = TRUE,       
  se = TRUE,          # Enable bootstrapping
  nboots = 100,       
  parallel = TRUE,
  min.T0 = 12,        # Ensure at least 12 pre-treatment periods
  na.rm = TRUE
)

out
plot(out, main = "Estimated ATT")
plot(out, type = "factors", xlab = "Time")


# Create a list of 'clean' repo IDs with zero NAs
active_repos <- caf %>%
  group_by(repo_sha_id, repo_group_id) %>%
  summarize(missing = sum(is.na(contributor_absence_factor))) %>%
  filter(missing == 0) %>%
  pull(repo_sha_id)

# Filter your main dataframe
caf_clean <- caf %>%
  filter(repo_sha_id %in% active_repos)

caf_clean %>%
  group_by(repo_group_id) %>%
  summarise(n=n())

sum(is.infinite(caf_clean$contributor_absence_factor))

caf_clean <- caf_clean %>%
  group_by(repo_sha_id) %>%
  filter(sd(contributor_absence_factor, na.rm = TRUE) > 0) %>%
  ungroup()

panelview(contributor_absence_factor ~ period_treated + oc_funding,
          data = caf_clean,
          index = c("repo_sha_id", "time_period"),
          pre.post = TRUE)


syn <- multisynth(contributor_absence_factor ~ period_treated,
                unit = repo_sha_id,
                time = time_period,
                data = caf)

summary(syn)




