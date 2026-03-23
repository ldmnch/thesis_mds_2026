library(tidyverse)
library(readxl)

source("src/model_plotting_and_formatting.R")

folder_path_root <- "data/final/"
specific_folder_path <- "2026-03-17_08-41-14"
full_path <- paste0(folder_path_root, specific_folder_path)

table <- compile_l2_improvement_table(full_path) 

table %>%
  mutate(model = map_chr(model, ~define_plot_title(.x, plot_type = "ATT")))
