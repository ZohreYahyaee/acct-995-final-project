# 00_setup.R — Load libraries and set data path

# Install pacman if not already installed
if (!require("pacman")) install.packages("pacman")

# Use pacman to load required packages
pacman::p_load(
  # Visualization
  ggplot2, patchwork, factoextra, formattable,
  
  # Data wrangling
  tidyverse, dplyr, tidyr, data.table, readr, lubridate, fuzzyjoin, zoo, janitor,
  
  # Modeling & ML
  caret, neuralnet, cluster, class, plm, fixest, imputeTS, gsynth,
  
  # ML Explanation
  vip, DALEX, DALEXtra, shapviz,
  
  # Panel / time
  tsibble, slider,
  
  # File I/O
  arrow,
  
  # Parallel Processing
  future, furrr, doFuture, doRNG
)

# Set up global path from .Renviron
# If not set up yet, uncomment and run the following line once
#usethis::edit_r_environ("project")

data_path <- Sys.getenv("DATA_PATH")
cat(" Data path is set to:", data_path, "\n")

