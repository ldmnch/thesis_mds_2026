library(tidyverse)
library(readxl)

source("src/model_plotting_and_formatting.R")

# Set the path to the folder containing the .csv files
folder_path_root <- "data/final/"
specific_folder_path <- "2026-04-12_12-40-27"
full_path <- paste0(folder_path_root, specific_folder_path)

# List all .csv files in the folder
result_files <- list.files(path = full_path, pattern = "\\.xlsx$", full.names = TRUE)
# Loop through each file, create a plot and save it 

for (test_file_path in result_files) {
  
  plot_coefficients(test_file_path, title = define_plot_title(test_file_path)) 
  
  ggsave(filename = paste0("plots/",specific_folder_path,"/", 
                           tools::file_path_sans_ext(basename(test_file_path)), ".png"), 
         width = 8, height = 6,
         create.dir = TRUE)
  
  }
