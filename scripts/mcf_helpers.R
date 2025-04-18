# This file will export functions to be used in the experiment
# run_simulation is the main function, others are helpers
library(rlemon)
library(dplyr)
source("scripts/data_gen.R")
source("scripts/networks.R")

time_mcf <- function(data, method, mc) {
  treated_scores <- data |> filter(Z == 1) |> pull(score)
  control_scores <- data |> filter(Z == 0) |> pull(score)

  M <- outer(treated_scores, control_scores, function(x, y) abs(x - y))


  # Before determining the minimum cost, we calculate the max flow
  mf <- max_controls(M, mc)

  # Then, we generate the network
  nwk <- generate_mincost_network(M, max.controls = mc, total.controls = mf)

  # Now we can run the min cost flow algorithm
  # We do it 10 times to improve precision of results
  # t1 <- Sys.time()
  # for (i in 1:10){
  #   MinCostFlow(
  #     nwk[["from_nodes"]],
  #     nwk[["to_nodes"]],
  #     nwk[["capacity"]],
  #     nwk[["cost"]],
  #     nodeSupplies = nwk[["supply"]],
  #     numNodes = nwk[["numNodes"]],
  #     algorithm = method
  #   )
  # }
  # t2 <- Sys.time()
  # as.numeric(t2 - t1, units = "secs") / 10
  t1 <- Sys.time()
  MinCostFlow(
    nwk[["from_nodes"]],
    nwk[["to_nodes"]],
    nwk[["capacity"]],
    nwk[["cost"]],
    nodeSupplies = nwk[["supply"]],
    numNodes = nwk[["numNodes"]],
    algorithm = method
  )
  t2 <- Sys.time()
  as.numeric(t2 - t1, units = "secs")
  
}

run_simulation <- function(params, grid_type, n_sims) {
  # Extract the parameters from the data frame
  n <- params$n
  p <- params$p
  mc <- params$mc
  results <- matrix(NA, nrow = n_sims, ncol = 4)
  for (i in 1:n_sims) {
    if (grid_type == "propensity") {
      a <- params$a
      b <- params$b
      bounded_params <- list(
        treated = data.frame(a = a, b = b),
        control = data.frame(a = b, b = a)
      )
      cont_params <- list(
        treated = data.frame(mean = a, sd = b),
        control = data.frame(mean = a, sd = b)
      )
      n_treated <- 0
      n_control <- 0
      while(min(n_treated, n_control) == 0) {
        data <- gen_data(n, p, bounded_params, cont_params)
        n_treated <- data |> pull(Z) |> sum()
        n_control <- n - n_treated
      }
      data <- data |>
        select(-cont_1) |>
        rename(score = bounded_1)
    } else if (grid_type == "prognostic") {
      sd <- params$sd
      mu <- params$mu
      bounded_params <- list(
        treated = data.frame(a = sd, b = sd),
        control = data.frame(a = sd, b = sd)
      )
      cont_params <- list(
        treated = data.frame(mean = mu, sd = sd),
        control = data.frame(mean = mu, sd = sd)
      )
      n_treated <- 0
      n_control <- 0
      while(min(n_treated, n_control) == 0) {
        data <- gen_data(n, p, bounded_params, cont_params)
        n_treated <- data |> pull(Z) |> sum()
        n_control <- n - n_treated
      }
      data <- data |>
        select(-cont_1) |>
        rename(score = bounded_1)
    } else {
      stop("Invalid grid type. Must be either 'propensity' or 'prognostic'.")
    }
  
    # Run the MCF algorithm and measure the time taken for each method
    simplex_time <- time_mcf(data, mc, method = "NetworkSimplex")
    cost_time <- time_mcf(data, mc, method = "CostScaling")
    capacity_time <- time_mcf(data, mc, method = "CapacityScaling")
    cycle_time <- time_mcf(data, mc, method = "CycleCancelling")
    
    results[i, ] <- c(simplex_time, cost_time, capacity_time, cycle_time) 
  }
  results_avg <- colMeans(results, na.rm = TRUE)
  
  list(
    simplex_time = results_avg[1], #simplex_time,
    cost_time = results_avg[2],#cost_time,
    capacity_time = results_avg[3],#capacity_time,
    cycle_time = results_avg[4]#cycle_time
  )
}