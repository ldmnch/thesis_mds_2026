library(tidyverse)

# ── 1. Data loading ────────────────────────────────────────────────────────────

load_repos <- function(path = "data/proc/experiment_repos_with_funding.csv",
                       filter_repos = getOption("experiment_repos")) {
  df <- read_csv(path) 
  
  if (!is.null(filter_repos)) {
    df <- df %>% filter(repo %in% filter_repos | repo_group_id == 204)
    
    summary <- df %>%
      group_by(repo_group_id) %>%
      summarise(n_repos = n_distinct(repo), .groups = "drop")
    
    print(summary)
  }
  
  df
}

load_table <- function(con, table) {
  as_tibble(tbl(con, Id(schema = "public", table = table))) %>%
    #drop duplicates
    distinct()
}

# ── 2. Core pipeline steps ─────────────────────────────────────────────────────

#' Aggregate raw events into quarterly counts
#' @param df         Raw events table
#' @param id_col     Repo identifier column (string)
#' @param date_col   Event date column (string)
#' @param target_col Name for the aggregated count column (string)
#' @param start_date Filter events before this date
aggregate_quarterly <- function(df, id_col, date_col, target_col,
                                start_date = "2015-01-01") {
  df %>%
    filter(.data[[date_col]] >= as.Date(start_date)) %>%
    mutate(floor_month = as.Date(floor_date(.data[[date_col]], "quarter"))) %>%
    group_by(.data[[id_col]], floor_month) %>%
    summarise("{target_col}" := n(), .groups = "drop")
}

#' Expand to a complete quarterly grid and join repo metadata
#' @param df         Aggregated quarterly df
#' @param repos      Repos metadata df
#' @param id_col     Repo identifier column in df (string)
#' @param repo_id_col Repo identifier column in repos (string)
#' @param target_col Count column to fill with 0s
#' @param start_date,end_date Quarter sequence boundaries
complete_quarterly_grid <- function(df, repos, id_col, repo_id_col, target_col,
                                    start_date = "2015-01-01",
                                    end_date   = "2025-12-01") {
  all_quarters <- seq(as.Date(start_date), as.Date(end_date), by = "quarter")
  
  fill_vals <- set_names(list(0), target_col)
  
  df %>%
    complete(
      "{id_col}" := .data[[id_col]],   # keep existing id values
      floor_month = all_quarters,
      fill = fill_vals
    ) %>%
    inner_join(repos, by = set_names(repo_id_col, id_col)) %>%
    mutate(time_period = as.numeric(as.factor(floor_month)))
}

#' Drop repos with zero activity across all periods
remove_ghost_repos <- function(df, id_col, target_col) {
  df %>%
    group_by(.data[[id_col]]) %>%
    filter(sum(.data[[target_col]]) > 0) %>%
    ungroup()
}

#' Add DiD and control variables
#' @param log_target_col Name for the log-transformed target (string)
enrich_variables <- function(df, id_col, target_col, log_target_col) {
  df %>%
    group_by(.data[[id_col]]) %>%
    mutate(
      repo_group_id              = as.integer(repo_group_id),
      "{log_target_col}"        := log1p(.data[[target_col]]),
      treated                    = if_else(repo_group_id < 200, 1L, 0L),
      period_treated             = if_else(
        repo_group_id < 200 & floor_month >= sta_start_date,
        1L, 0L),
      oc_funding                 = if_else(
        !is.na(oc_funding_start_at) & floor_month >= oc_funding_start_at,
        1L, 0L),
      size_x_period_treated      = size * time_period,
      stargazers_x_period_treated = stargazers_count * time_period
    ) %>%
    ungroup()
}

# ── 3. Master pipeline ─────────────────────────────────────────────────────────

#' End-to-end pipeline for a single outcome variable
#' @examples
#' releases_frequency <- build_experiment_panel(
#'   raw_df       = releases,
#'   repos        = repos,
#'   id_col       = "repo_sha_id",
#'   date_col     = "published_at",
#'   target_col   = "n_releases",
#'   repo_id_col  = "sha_id"
#' )
build_experiment_panel <- function(raw_df, repos,
                                   id_col      = "repo_sha_id",
                                   date_col,
                                   target_col,
                                   repo_id_col = "sha_id",
                                   start_date  = "2015-01-01",
                                   end_date    = "2025-12-01") {
  
  log_target_col <- paste0("log_", target_col)
  
  panel_df <- raw_df %>%
    aggregate_quarterly(id_col, date_col, target_col, start_date) %>%
    complete_quarterly_grid(repos, id_col, repo_id_col, target_col,
                            start_date, end_date) %>%
    remove_ghost_repos(id_col, target_col) %>%
    enrich_variables(id_col, target_col, log_target_col)
  
  outlier_units <- panel_df %>%
    filter(treated == 0) %>%
    group_by(.data[[id_col]]) %>%
    summarize(sd_val = sd(.data[[log_target_col]], na.rm = TRUE), .groups = "drop") %>%
    filter(sd_val > quantile(sd_val, 0.98, na.rm = TRUE)) %>%
    pull(all_of(id_col)) # Pull the actual IDs
  
  panel_df %>%
    filter(!(.data[[id_col]] %in% outlier_units))  
}