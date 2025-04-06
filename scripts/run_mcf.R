# This file will export functions to be used in the experiment
# run_simulation is the main function, others are helpers



run_simulation <- function(params, grid_type){
  # Extract the parameters from the data frame
  n <- params$n
  p <- params$p
  sd <- params$sd
  mu <- params$mu
  beta <- params$beta
  tau <- params$tau
  beta_tau <- params$beta_tau
  sigma <- params$sigma
  
  # Generate the data
  bounded_params <- list(
    treated = data.frame(a = params$a_treated, b = params$b_treated),
    control = data.frame(a = params$a_control, b = params$b_control)
  )
  
  cont_params <- list(
    treated = data.frame(mean = mu, sd = sd),
    control = data.frame(mean = mu, sd = sd)
  )
  
  data <- gen_data(
    n, p, bounded_params, cont_params,
    gen_outcomes = FALSE, sigma, beta, tau, beta_tau
  )
  
  # Run the MCF algorithm and measure the time taken for each method
  simplex_time <- system.time({
    mcf_simplex(data)
  })[3]
  
  cost_time <- system.time({
    mcf_cost(data)
  })[3]
  
  capacity_time <- system.time({
    mcf_capacity(data)
  })[3]
  
  cycle_time <- system.time({
    mcf_cycle(data)
  })[3]
  
  c(simplex_time, cost_time, capacity_time, cycle_time)
}

