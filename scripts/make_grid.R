# This file will produce the CSV encoding the grid on which the experiments will be run.
# This grid will be used to generate the data for the experiments.


# First, we simulate the continuous covariates
# The necessary parameters are:
# n: the number of samples
# p: probability of being treated
# mu: the mean of the control distribution
# sd: the standard deviation of the control distribution
# n, p, and sd will vary on fixed ranges
# we will allow mu to range in a manner dependent on n, p, and sd
# Namely, we let mu range from 0 to mu_upper(n, p, sd)
# where mu_upper(n, p, sd) = mu* is such that
# P(max(X in treatment) >  min(X in control)) = 0.95
n_lower <- 100
n_lower_exp <- log10(n_lower)
n_upper <- 1e5
n_upper_exp <- log10(n_upper)
n_size <- 10
n <- round(10^seq(from = n_lower_exp, to = n_upper_exp, length.out = n_size))

p_lower <- 0.05
p_upper <- 0.95
p_size <- 10
p <- seq(from = p_lower, to = p_upper, length.out = p_size)


sd_lower <- 1/5
sd_upper <- 5
sd_size <- 10
sd <- seq(from = sd_lower, to = sd_upper, length.out = sd_size)

# Now we implement the calculations to determine mu_upper
integrand <- function(x, mu, sigma, nt, nc) {
  term1 <- dnorm(x)
  term2 <- pnorm(x)^(nt - 1)
  term3 <- (1 - pnorm((x - mu) / sigma))^nc
  nt * term1 * term2 * term3
}

prob_overlap <- function(mu, sigma, n, p, simple = FALSE) {
  if (simple) {
    result <- list(value = 1)
    tryCatch({
      result <- integrate(
        integrand,
        mu = mu,
        sigma = sigma,
        nt = 1,
        nc = n - 1,
        lower = -Inf,
        upper = Inf,
        rel.tol = 1e-10
      )
    }, error = function(e) {
      result <- list(value = 0)
    }, warning = function(w) {
      result <- list(value = 0)
    })
    return(1 - result$value)
  }
  probs <- numeric(length(1:(n - 1)))
  for (i in 1:(n - 1)) {
    #print("inner call")
    #print(i)
    #print(mu)
    # For large n we need to tamp down the tolerance value to avoid overflow
    tryCatch({
      result <- integrate(
        integrand,
        mu = mu,
        sigma = sigma,
        nt = i,
        nc = n - i,
        lower = -Inf,
        upper = Inf,
        rel.tol = 1e-10
      )
    }, error = function(e) {
      result <- prev_result
    })
    integral_value <- result$value
    probs[i] <- (1 - integral_value) * dbinom(i, n, p)
    prev_result <- result
  }
  sum(probs) / (1 - dbinom(0, n, p) - dbinom(n, n, p))
}

mu_upper <- function(n, p, sd, simple = FALSE) {
  #print("mu upper call")
  #print(paste("n:", n))
  #print(paste("p:", p))
  #print(paste("sd:", sd))
  x <- 0
  step_size <- 0.1
  prob_overlap <- prob_overlap(x, sd, n, p, simple = TRUE)
  if (prob_overlap < 0.99) {
    return(NA)
  }
  while (prob_overlap > 0.99) {
    x <- x + step_size
    prob_overlap <- prob_overlap(x, sd, n, p, simple = TRUE)
  }
  x
}
#mu_upper(46416, 0.15, 0.2)
#integrate(integrand, mu = 5, sigma = 0.2, nt = 1176, nc = 46416 - 1176, lower = -Inf, upper = Inf, rel.tol = 1e-9)

# Now we create the grid
first_order_grid <- expand.grid(
  n = n,
  p = p,
  sd = sd
)

mu_size <- 10

# For each of these values in the first order grid
# we're going to have mu_size values of mu
# So our second order grid copies the first order grid mu_size times
second_order_grid <- first_order_grid
for (i in 1:(mu_size - 1)){
  second_order_grid <- rbind(second_order_grid, first_order_grid)
}
second_order_grid$mu <- NA

library(tibble)
tibble(second_order_grid)
# Now we need to fill in the mu values
# We will do this by looping over the first order grid
# and filling in the mu values for each row of the second order grid
# We will use the mu_upper function to determine the upper bound for mu
# and we will use seq to generate mu_size values from 0 to mu_upper
for (i in 1:nrow(first_order_grid)) {
  row <- first_order_grid[i, ]
  n <- row[1]
  p <- row[2]
  sd <- row[3]
  relevant_rows <- (second_order_grid$n == n) & (second_order_grid$p == p) & (second_order_grid$sd == sd)
  max_mu <- mu_upper(n, p, sd, simple = TRUE)
  if (is.na(max_mu)) {
    second_order_grid[relevant_rows, "mu"]  <- rep(NA, times = mu_size)
  } else {
    mu <- seq(from = 0, to = max_mu, length.out = mu_size)
    second_order_grid[relevant_rows, "mu"]  <- mu
  }
}
sum((second_order_grid$n == 100) & (second_order_grid$p == 0.35) & (second_order_grid$sd == 0.2))
print(second_order_grid)



mc_lower <- 1
mc_upper <- 10
mc_size <- 10
mc <- 1:5