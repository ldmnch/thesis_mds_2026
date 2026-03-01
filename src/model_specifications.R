library(augsynth)
library(gsynth)
library(panelView)

train_gsynth_model <- function(data, 
                               index, 
                               target, 
                               treatment = "period_treated",
                               force = "two-way", 
                               r = c(0, 10),      # lowered from c(0, 10)
                               CV = TRUE, 
                               se = TRUE, 
                               nboots = 500, 
                               parallel = TRUE, 
                               min.T0 = 12,
                               inference = "parametric") {  # added - more appropriate for small N
  
  formula <- as.formula(paste(target, "~", treatment))
  
  # Safeguard: cap r max based on available control units
  n_control <- length(unique(data$repo_sha_id[data[[treatment]] == 0]))
  r_max <- min(r[2], floor(n_control / 5))  # rule of thumb: don't exceed N_control / 5
  r_safe <- c(r[1], max(r_max, r[1]))
  
  model <- gsynth(
    formula = formula,
    data = data,
    index = index,
    force = force,
    r = r_safe,
    CV = CV,
    se = se,
    nboots = nboots,
    parallel = parallel,
    min.T0 = min.T0,
    inference = inference,   # parametric is safer with ~40 treated units
    na.rm = TRUE
  )
  
  return(model)
}

train_augsynth_model <- function(data, 
                                 target, 
                                 treatment = "period_treated",
                                 #covariates = "oc_funding + stargazers_count + size",
                                 unit, 
                                 time) {
  
  unit <- rlang::ensym(unit)
  time <- rlang::ensym(time)
  
  formula <- as.formula(paste(target, "~", treatment
                              #, "|", covariates
                              ))
  
  model <- multisynth(
    form = formula,
    unit = !!unit,
    time = !!time,
    data = data
  )
  
  return(model)
}
