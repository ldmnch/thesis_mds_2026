library(tidyverse)
library(dotenv)
library(readxl)
library(lubridate)
# load functions from another code module in r
source("src/setup.R")

repos <- as_tibble(tbl(con, Id(schema = "public", table = "repo_names")))
repo_groups <- as_tibble(tbl(con, Id(schema = "public", table = "repo_groups")))
repo_metadata <- as_tibble(tbl(con, Id(schema = "public", table = "repo_metadata")))

sta_funding <- read_excel("data/raw/sta_funding_data.xlsx") 
oc_funding <- as_tibble(tbl(con, Id(schema = "public", table = "funding_data")))

## Prepro STA funding to repo group ids 

sta_funding <- sta_funding %>%
  mutate(
    Name = str_to_lower(Name),
    repo_group_id = as.integer(case_when(
    str_detect(Name, "numfocus") ~ 104,
    str_detect(Name, "pypi") ~ 101,
    str_detect(Name, "curl") ~ 105,
    str_detect(Name, "ruby") ~ 102)),
    `Start Date` = dmy(`Start Date`),
    `End Date` = dmy(`End Date`),
    # 1. Rename currency (will always be EUR)
    sta_funding_currency = "EUR",
    
    # 2. Clean the amount:
    # Remove the currency symbol and any whitespace
    # Remove the thousand separator (.)
    # Replace the decimal comma (,) with a dot (.)
    sta_funding_amount = `Total Amount` %>%
      str_remove_all("[^0-9.,]") %>% 
      str_replace_all("\\.", "") %>% 
      str_replace(",", ".") %>%
      as.numeric()
    ) %>%
  rename(
    sta_funding_name = Name,
    sta_start_date = `Start Date`,
    sta_end_date = `End Date`
  ) %>%
  select(-`Total Amount`)

sta_funding <- sta_funding %>%
  drop_na(repo_group_id) %>%
  arrange(repo_group_id, sta_start_date)

sta_funding <- sta_funding %>%
  group_by(repo_group_id) %>%
  summarise(
    sta_start_date = min(sta_start_date),
    sta_end_date = max(sta_end_date),
    sta_funding_amount = sum(sta_funding_amount),
    sta_funding_currency = first(sta_funding_currency)
    ) 

# Keep only relevant columns from OC funding

oc_funding <- oc_funding %>%
  select(repo_sha_id, id, collective_created_at, total_donations, currency) %>%
  rename(
    oc_funding_id = id,
    oc_funding_start_at = collective_created_at,
    oc_funding_amount = total_donations,
    oc_funding_currency = currency
  ) %>%
  mutate(
    oc_funding_start_at = as_date(oc_funding_start_at)
  )

### prepro repos. 
## first i keep only the ones who will go into the experiment 

repos <- repos %>%
  filter(repo_group_id != 203)

repos %>%
  group_by(repo_group_id) %>%
  summarise(n = n())

repos_with_funding <- repos %>% left_join(
  oc_funding,
  by = c("sha_id" = "repo_sha_id")) 

repos_with_funding <- repos_with_funding %>% 
  left_join(sta_funding) 

# add a column "non free" if the repo owner contains: google, aws, databricks or facebook 

repos_with_funding <- repos_with_funding %>%
  mutate(
    private_owned = if_else(
      str_detect(owner, "google|aws|databricks|facebook|microsoft"),
      "private_owned",
      "non_private"
    ),
    funding = if_else(
      !is.na(oc_funding_amount) | !is.na(sta_funding_amount)| private_owned == "private_owned",
      TRUE,
      FALSE
    )
  )

repos_with_funding <- repos_with_funding %>%
  left_join(repo_metadata)

repos_with_funding %>%
  group_by(funding) %>%
  summarise(count = n())

repos %>%
  group_by(repo_group_id) %>%
  summarise(n = n())

write_csv(repos_with_funding, "data/proc/experiment_repos_with_funding.csv")
