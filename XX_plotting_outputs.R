library(tidyverse)
library(readxl)

source("src/model_plotting_and_formatting.R")

# Set the path to the folder containing the .csv files
folder_path <- "data/final"
# List all .csv files in the folder
gsynth_files <- list.files(path = folder_path, pattern = "\\.xlsx$", full.names = TRUE)

augsynth_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop through each file, create a plot and save it 

for (test_file_path in c(gsynth_files, augsynth_files)) {
  
  plot_coefficients(test_file_path, title = define_plot_title(test_file_path)) 
  
  ggsave(filename = paste0("plots/", tools::file_path_sans_ext(basename(test_file_path)), ".png"), 
         width = 8, height = 6)
  
  }
