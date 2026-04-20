library(augsynth)
library(gsynth)
library(panelView)

train_gsynth_model <- function(data, 
                               index, 
                               target, 
                               treatment = "period_treated",
                               force = "two-way", 
                               r = c(0, 10),    
                               CV = TRUE, 
                               se = TRUE, 
                               nboots = 1000, 
                               parallel = TRUE, 
                               min.T0 = 12,
                               inference = "parametric") {  
  
  formula <- as.formula(paste(target, "~", treatment))
  
  #  cap r max based on available control units
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
    inference = inference,   
    na.rm = TRUE
  )
  
  return(model)
}

train_augsynth_model <- function(data, 
                                 target, 
                                 treatment = "period_treated",
                                 covariates = FALSE,
                                 covariate_str = "oc_funding, stargazers_count, size",
                                 unit, 
                                 time,
                                 n_factors_range = 1:10) {
  
  unit_quo  <- enquo(unit)
  time_quo  <- enquo(time)
  
  if (!isFALSE(covariates)) {
    covariate_str <- paste(covariates, collapse = " + ")
    formula <- as.formula(paste(target, "~", treatment, "|", covariate_str))
    data <- data %>% drop_na(size, stargazers_count)
  } else {
    formula <- as.formula(paste(target, "~", treatment))
  }
  
  message("Running Cross-Validation for latent factors...")
  
  pc_results <- sapply(n_factors_range, function(f) {
    m <- multisynth(
      form    = formula,
      unit    = !!unit_quo,   
      time    = !!time_quo,
      data    = data,
      n_factors = f
    )
    return(m$params$PC)
  })
  
  best_f <- n_factors_range[which.min(pc_results)]
  message(paste("Optimal factors chosen (Min PC):", best_f))
  
  final_model <- multisynth(
    form      = formula,
    unit      = !!unit_quo,
    time      = !!time_quo,
    data      = data,
    n_factors = best_f
  )
  
  final_model$ic_table <- data.frame(factors = n_factors_range, IC = pc_results)
  return(final_model)
}
