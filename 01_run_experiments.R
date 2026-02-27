library(tidyverse)
library(dotenv)

# load functions from another code module in r
source("src/setup.R")
source("src/build_experiment_data.R")
source("src/model_specifications.R")
source("src/model_plotting_and_formatting.R")

# List experiment scripts
experiment_files <- list.files(
  "experiments",
  pattern = "\\.R$",
  full.names = TRUE
)

# Run them
invisible(lapply(experiment_files, source))
