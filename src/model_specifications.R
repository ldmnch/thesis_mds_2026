library(augsynth)
library(gsynth)
library(panelView)

train_gsynth_model <- function(data, 
                               index, 
                               target, 
                               treatment = "period_treated",
                               #covariates = "oc_funding + stargazers_x_period_treated + size_x_period_treated",
                               force = "two-way", 
                               r = c(0, 10), 
                               CV = TRUE, 
                               se = TRUE, 
                               nboots = 500, 
                               parallel = TRUE, 
                               min.T0 = 12) {
  
  
  
  formula <- as.formula(paste(target, "~", treatment))
  
  model <- gsynth(
    formula = formula,
    data = data,
    index = index,
    force = force,
    r = r,
    CV = CV,
    se = se,
    nboots = nboots,
    parallel = parallel,
    min.T0 = min.T0,
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
