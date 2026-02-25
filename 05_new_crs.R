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
source("src/model_plotting_and_formatting.R")
#remove scientific notation
options(scipen = 999)

repos <- read_csv("data/proc/experiment_repos_with_funding.csv")

prs <- as_tibble(tbl(con, Id(schema = "public", table = "prs")))

merged_crs <- prs %>%
  mutate(
    floor_month = as.Date(floor_date(created_at, "quarter")),
  ) %>%
  group_by(repo_sha_id, floor_month)  %>%
  summarise(
    n_crs = sum(!is.na(created_at)),
    .groups = "drop"
  ) %>%
  filter(floor_month >= as.Date("2020-01-01") & floor_month <= as.Date("2025-12-01")) 


all_months <- seq(as.Date("2020-01-01"), as.Date("2025-12-01"), by = "quarter")
all_repos <- unique(merged_crs$repo_sha_id)

merged_crs <- merged_crs %>%
  # Use complete to force the full grid for EVERY repo
  complete(repo_sha_id = all_repos, floor_month = all_months, 
           fill = list(n_crs = 0)) %>%
  arrange(repo_sha_id, floor_month) %>%
  mutate(
    time_period = as.numeric(as.factor(floor_month))
  )

merged_crs <- merged_crs %>%
  inner_join(repos, by = c("repo_sha_id" = "sha_id")) %>%
  group_by(repo_sha_id) %>%
  mutate(
    log_n_crs = log1p(n_crs),
    repo_group_id = as.integer(repo_group_id),
    treated = if_else(repo_group_id < 200, 1L, 0L),
    period_treated = if_else(repo_group_id < 200 & floor_month >= sta_start_date, 1L, 0L),
    oc_funding = if_else(
      !is.na(oc_funding_start_at) & floor_month >= oc_funding_start_at, 
      1L, 0L
    )
  ) %>%
  ungroup()

panelview(log_n_crs ~ period_treated + oc_funding,
          data = merged_crs,
          index = c("repo_sha_id", "floor_month"),
          pre.post = TRUE, 
          type = "outcome")


ggplot(merged_crs, aes(x = floor_month, y = log_n_crs, color = as.factor(treated))) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Average Number PRs Over Time by Treatment Status",
       x = "Time (Quarterly)", y = "Average Number of Merged PRs") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red"), labels = c("Control", "Treated")) +
  theme(legend.title = element_blank())

out <- gsynth(
  formula = log_n_crs ~ period_treated + oc_funding,  
  data = merged_crs,
  index = c("repo_sha_id", "time_period"), 
  force = "two-way", 
  r = c(0,5),         
  CV = TRUE,       
  se = TRUE,          # Enable bootstrapping
  nboots = 200,       # Start with 200; increase to 1000 for final paper
  parallel = TRUE,
  min.T0 = 12,        # Ensure at least 12 pre-treatment periods
  na.rm = TRUE
)

out 
plot(out, type = "counterfactual", main = "Counterfactuals (MC)")
plot(out, main = "Estimated ATT")
plot(out, type = "raw")

plot(out, type = "counterfactual", id = "some_repo_id")

syn <- multisynth(log_n_crs ~ period_treated | oc_funding,
                unit = repo_sha_id,
                time = time_period,
                data = merged_crs)

summary(syn)

extract_effects_augsynth(syn)
