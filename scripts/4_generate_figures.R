# This file will take in the results under `results/` and generate figures
# For the report, we will generate tables and plots that summarize the results
# In particular, we are interested in the following:
# - The overall relative performance of the algorithms
# - The performance as a function of:
#   - The number of samples: n
#   - The probability of being treated: p
#   - The maximum number of controls: mc
#   - The imbalance: `imbalance`
# All of those "performance as a function of" plots will be generated
# For any plot/table, we will generate two version:
# - One for the propensity score simulation
# - One for the prognostic score simulation
library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)

propensity_results <- readRDS("results/propensity_results.rds")
prognostic_results <- readRDS("results/prognostic_results.rds")

# =====================
# HELPER FUNCTIONS
# =====================

# Helper function to reshape data from wide to long format
reshape_data <- function(data) {
  data %>%
    pivot_longer(
      cols = c(simplex_times, cost_times, cycle_times),
      names_to = "algorithm",
      values_to = "time"
    ) %>%
    mutate(algorithm = gsub("_times", "", algorithm))
}

make_function_of_plot <- function(column, setting) {
  # Get and reshape data
  data <- if(setting == "propensity") propensity_results else prognostic_results
  data_long <- reshape_data(data)
  
  # Calculate means for each combination of column value and algorithm
  data_summary <- data_long %>%
    group_by(!!sym(column), algorithm) %>%
    summarise(
      mean_time = mean(time),
      .groups = 'drop'
    )
  
  dir.create("figures/plots", recursive = TRUE, showWarnings = FALSE)
  
  p <- ggplot(data_summary, 
              aes(x = factor(!!sym(column)), 
                  y = mean_time, 
                  color = algorithm, 
                  group = algorithm)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    labs(x = column,
         y = "Mean Time (seconds)",
         title = paste("Mean Time vs", column, "for", setting, "setting")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    filename = paste0("figures/plots/", setting, "_", column, ".png"),
    plot = p,
    width = 8,
    height = 6
  )
}

make_scatter_plot <- function(column, setting) {
  # Get and reshape data
  data <- if(setting == "propensity") propensity_results else prognostic_results
  data_long <- reshape_data(data)
  
  dir.create("figures/plots/scatter", recursive = TRUE, showWarnings = FALSE)
  
  p <- ggplot(data_long,
              aes(x = !!sym(column),
                  y = time, 
                  color = algorithm,
                  group = algorithm)) +
    geom_point(alpha = 0.3, size = 1) +
    geom_smooth(method = "loess", se = TRUE, linewidth = 1) +
    ylim(c(0, 0.005)) +
    theme_minimal() +
    labs(x = column,
         y = "Time (seconds)",
         title = paste("Running Time vs", column, "for", setting, "setting"))
  
  ggsave(
    filename = paste0("figures/plots/scatter/", setting, "_", column, ".png"),
    plot = p,
    width = 8,
    height = 6
  )
}

# =====================
# TABLES
# =====================

dir.create("figures/tables", recursive = TRUE, showWarnings = FALSE)

for (setting in c("propensity", "prognostic")) {
  data <- if(setting == "propensity") propensity_results else prognostic_results
  
  # Calculate statistics for each algorithm (excluding capacity)
  stats <- data.frame(
    algorithm = c("simplex", "cost", "cycle", "capacity")
  )
  
  # Calculate mean and sd
  stats$time_stats <- sapply(paste0(stats$algorithm, "_times"), function(col) {
    sprintf("%.8f (%.8f)", mean(data[[col]]), sd(data[[col]]))
  })
  
  # Calculate best percentage with proper tie handling (excluding capacity)
  times_matrix <- data[, paste0(stats$algorithm, "_times")]
  best_pct <- sapply(1:ncol(times_matrix), function(i) {
    ties <- times_matrix == apply(times_matrix, 1, min)
    mean(ties[,i] / rowSums(ties)) * 100
  })
  stats$best_pct <- best_pct
  
  # Create markdown table
  md_content <- c(
    paste("##", setting, "setting"),
    "",
    "| Algorithm | Time (mean ± sd) | Best Performance (%) |",
    "|-----------|-----------------|-------------------|",
    paste("|", stats$algorithm, "|", stats$time_stats, "|", 
          sprintf("%.1f", stats$best_pct), "|", collapse = "\n"),
    "\n"
  )
  
  cat(paste(md_content, collapse = "\n"), 
      file = "figures/tables/overall_performance.md", 
      append = setting != "propensity")
}

# =====================
# PLOTS
# =====================

# PLOTS 1 and 2: time as a function of n for both settings
make_function_of_plot("n", "propensity")
make_function_of_plot("n", "prognostic")

# PLOT 3 and 4: time as a function of p for both settings
make_function_of_plot("p", "propensity")
make_function_of_plot("p", "prognostic")

# PLOT 4 and 5: time as a function of mc for both settings
make_function_of_plot("mc", "propensity")
make_function_of_plot("mc", "prognostic")

# PLOT 6 and 7: time as a function of imbalance for both settings
make_function_of_plot("imbalance", "propensity")
make_function_of_plot("imbalance", "prognostic")

# PLOT 8 and 9: time as a function of rel_mc for both settings
make_function_of_plot("rel_mc", "propensity")
make_function_of_plot("rel_mc", "prognostic")

# New scatter plots for mc and rel_mc
# PLOT 10 and 11: scatter plots for mc for both settings
make_scatter_plot("mc", "propensity")
make_scatter_plot("mc", "prognostic")

# PLOT 12 and 13: scatter plots for rel_mc for both settings
make_scatter_plot("rel_mc", "propensity")
make_scatter_plot("rel_mc", "prognostic")