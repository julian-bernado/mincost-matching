# This file is used to perform a simulation study to compare the performance of
# different matching methods in terms of their runtime, as a function of the
# DGP parameters.
# The simulation study is performed by generating data using the function
# `gen_data` and then comparing runtime of different matching methods.

source("scripts/data_gen.R")

# Set the seed for reproducibility
set.seed(606)

### Define the parameter grid for the simulation study
# Note that because the outcome distribution is not relevant to the matching 
# algorithms, we need not specify sigma, beta, tau, or beta_tau in the parameter
# grid.
n_vals <- c(10, 100, 1000, 10000, 100000)
prob_treated_vals <- c(0.01, 0.1, 0.25, 0.5)
# List of possible values for the parameters of the bounded covariates
bounded_ab_vals <- c(0.1, 0.5, 1, 5)
# All possible combinations of parameter values for treatment and control for a 
# single bounded covariate
bounded_param_vals <- expand.grid(
  bounded_ab_vals, bounded_ab_vals, bounded_ab_vals, bounded_ab_vals
)
p_bounded_vals <- c(1, 5, 10, 100, 1000)
# List of possible means for the continuous covariates
cont_mean_vals <- c(0, 1, 5, 10)
# List of possible standard deviations for the continuous covariates
cont_sd_vals <- c(0.1, 1, 5, 10)
# All possible combinations of parameter values for treatment and control for a
# single continuous covariate
cont_param_vals <- expand.grid(
  cont_mean_vals, cont_sd_vals, cont_mean_vals, cont_sd_vals
)
p_cont_vals <- c(1, 5, 10, 100, 1000)

# Experiment 1: See how runtime changes by algorithm as a function of sample 
#               size and probability of treatment.
#               For this experiment, we fix 5 continuous covariates, all of 
#               which are N(0,1) for both treated and control groups.
for(n in n_vals){
  for(prob_treated in prob_treated_vals){
    # Generate the data
    data <- gen_data(
      n = n, prob_treated = prob_treated, bounded_params = NULL, 
      cont_params = list(
        treated = data.frame(mean = rep(0, 5), sd = rep(1, 5)),
        control = data.frame(mean = rep(0, 5), sd = rep(1, 5))
      ),
      gen_outcomes = FALSE
    )
    
    # Perform matching using different algorithms
    # Note: The actual matching code will be added here
  }
}

# Experiment 2: See how runtime changes by algorithm as a function of sample
#               size and the bounded covariate distribution parameters.
#               For this experiment, we fix probability of treatment as 0.5 and
#               5 for the number of bounded covariates, but vary the parameters
#               of the beta distribution for the treated and control groups.
for(n in n_vals){
  for(i in nrow(bounded_param_vals)){
    # Generate the data
    data <- gen_data(
      n = n, prob_treated = 0.5, bounded_params = list(
        treated = data.frame(
          a = rep(bounded_param_vals[i,1], 5), b = rep(bounded_param_vals[i,2], 5)
        ),
        control = data.frame(
          a = rep(bounded_param_vals[i,3], 5), b = rep(bounded_param_vals[i,4], 5)
        )
      ), cont_params = NULL,
      gen_outcomes = FALSE
    )
    
    # Perform matching using different algorithms
    # Note: The actual matching code will be added here
  }
}

# Experiment 3: See how runtime changes by algorithm as a function of sample
#               size and the continuous covariate distribution parameters.
#               For this experiment, we fix probability of treatment as 0.5 and
#               5 for the number of continuous covariates, but vary the mean and
#               standard deviation of the normal distribution for the treated and
#               control groups.
for(n in n_vals){
  for(i in nrow(cont_param_vals)){
    # Generate the data
    data <- gen_data(
      n = n, prob_treated = 0.5, bounded_params = NULL, 
      cont_params = list(
        treated = data.frame(
          mean = rep(cont_param_vals[i,1], 5), sd = rep(cont_param_vals[i,2], 5)
        ),
        control = data.frame(
          mean = rep(cont_param_vals[i,3], 5), sd = rep(cont_param_vals[i,4], 5)
        )
      ),
      gen_outcomes = FALSE
    )
    
    # Perform matching using different algorithms
    # Note: The actual matching code will be added here
  }
}

# Experiment 4: See how runtime changes by algorithm as a function of sample
#               size and the number of bounded covariates.
#               For this experiment, we fix probability of treatment as 0.5 and
#               the parameters of the beta distribution for the treated and
#               control groups (to a uniform), but vary the number of bounded 
#               covariates.
for(n in n_vals){
  for(p_bounded in p_bounded_vals){
    # Generate the data
    data <- gen_data(
      n = n, prob_treated = 0.5, bounded_params = list(
        treated = data.frame(a = rep(1, p_bounded), b = rep(1, p_bounded)),
        control = data.frame(a = rep(1, p_bounded), b = rep(1, p_bounded))
      ), cont_params = NULL,
      gen_outcomes = FALSE
    )
    
    # Perform matching using different algorithms
    # Note: The actual matching code will be added here
  }
}

# Experiment 5: See how runtime changes by algorithm as a function of sample
#               size and the number of continuous covariates.
#               For this experiment, we fix probability of treatment as 0.5 and
#               the parameters of the normal distribution for the treated and
#               control groups (to N(0,1)), but vary the number of continuous
#               covariates.
for(n in n_vals){
  for(p_cont in p_cont_vals){
    # Generate the data
    data <- gen_data(
      n = n, prob_treated = 0.5, bounded_params = NULL, 
      cont_params = list(
        treated = data.frame(mean = rep(0, p_cont), sd = rep(1, p_cont)),
        control = data.frame(mean = rep(0, p_cont), sd = rep(1, p_cont))
      ),
      gen_outcomes = FALSE
    )
    
    # Perform matching using different algorithms
    # Note: The actual matching code will be added here
  }
}
