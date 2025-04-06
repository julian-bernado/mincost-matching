# This files takes one of the grids from `grids/`
# And runs the simulation for each row of the grid
# The results are saved in `results/`
library(dplyr)
library(readr)
source("data_gen.R")
source("run_mcf.R")

# Read in the argument passed in when running the file
arguments <- commandArgs(trailingOnly = TRUE)
grid_type <- as.character(arguments[1])

# Now, load the data
grid_path <- file.path("grids", paste0(grid_type, "_grid.csv"))
df <- read_csv(grid_path)
df$simplex_times <- NA
df$cost_times <- NA
df$capacity_times <- NA
df$cycle_times <- NA

# Now, we can run the simulation for each row of the grid
for (i in 1:nrow(df)) {
  params <- df[i, ]
  result <- run_simulation(params, grid_type = grid_type)
  df[i, c("simplex_times", "cost_times", "capacity_times", "cycle_times")] <- result
}

# Save the results
results_path <- file.path("results", paste0(grid_type, "_results.csv"))