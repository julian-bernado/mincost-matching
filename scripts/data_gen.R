# Function for generating data as a function of the parameters described in 
# notes/dgp_design.md.
# This function is used to generate the data for the simulation study.
#
# The DGP follows a linear functional form and can capture (1) pure noise,
# (2) a no treatment effect model, (3) a constant treatment effect model, and
# (4) a heterogeneous treatment effect model.
#
# The inputs for this function are:
# - n:              the number of individuals/observations.
# - prob_treated:   the probability of each individual being treated.
# - beta:           the vector capturing covariate effects.
# - tau:            the non-heterogeneous treatment effect.
# - beta_tau:       the vector capturing heterogeneous treatment effects of 
#                   covariates.
# - sigma:          the standard deviation of the error term.
# - bounded_params: bounded covariates are modeled as beta random variables.
#                   this contains parameters for the beta distribution of the 
#                   bounded covariates and is provided as a list of two 
#                   dataframes (one treated, one control), where each row 
#                   corresponds to the parameters for a single bounded covariate.
# - cont_params:    the parameters for the normal distribution of the continuous 
#                   covariates.
#                   this is provided as a list of two dataframes (one treated,
#                   one control), where each row corresponds to the parameters
#                   for a single continuous covariate.
gen_data <- function(
  n, prob_treated, beta, tau, beta_tau, sigma, bounded_params, cont_params
) {
  # Generate the treatment indicator
  Z <- rbinom(n = n, size = 1, p = prob_treated)
  
  # Generate the bounded covariates
  bounded_covs <- do.call(rbind, lapply(Z, FUN = function(Z_i){
    apply(bounded_params[[Z_i + 1]], MARGIN = 1, FUN = function(params_i){
      rbeta(1, params_i[1], params_i[2])
    })
  }))
  colnames(bounded_covs) <- paste0("bounded_", 1:ncol(bounded_covs))
  # Generate the continuous covariates
  cont_covs <- do.call(rbind, lapply(Z, FUN = function(Z_i){
    apply(cont_params[[Z_i + 1]], MARGIN = 1, FUN = function(params_i){
      rnorm(1, params_i[1], params_i[2])
    })
  }))
  colnames(cont_covs) <- paste0("cont_", 1:ncol(cont_covs))

  # Combine the covariates
  X <- cbind(bounded_covs, cont_covs)
  
  # Generate the error term
  error <- rnorm(n, 0, sigma)
  
  # Generate the outcome
  Y <- X %*% beta + tau * Z + X %*% beta_tau * Z + error
  
  # Return the data
  return(data.frame(Y = Y, Z = Z, X))
}