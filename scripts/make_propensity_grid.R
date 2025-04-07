# This file will produce the CSV encoding the grid on which the experiments will be run.
# This grid will be used to generate the data for the experiments.


# Here, we simulate the propensity scores
# The necessary parameters are:
# n: the number of samples
# p: probability of being treated
# a: beta shape parameter 1
# n, p, and a will vary on fixed ranges
# P(max(X in treatment) >  min(X in control)) = 0.95
n_lower <- 100
n_lower_exp <- log10(n_lower)
n_upper <- 1e5
n_upper_exp <- log10(n_upper)
n_size <- 10
n <- round(10^seq(from = n_lower_exp, to = n_upper_exp, length.out = n_size))

p_lower <- 0.05
p_upper <- 0.95
p_size <- 5
p <- seq(from = p_lower, to = p_upper, length.out = p_size)

a_lower <- 1
a_upper <- 50
a_size <- 5
a <- seq(from = a_lower, to = a_upper, length.out = a_size)

b_size <- 5

first_order_grid <- expand.grid(
  n = n,
  p = p,
  a = a
)

# For each of these values in the first order grid
# we're going to have mu_size values of mu
# So our second order grid copies the first order grid mu_size times
second_order_grid <- first_order_grid
for (i in 1:(b_size - 1)){
  second_order_grid <- rbind(second_order_grid, first_order_grid)
}
second_order_grid$b <- NA
second_order_grid$imbalance <- NA

# Now we need to fill in the b values
# We will do this by looping over the first order grid
# and filling in the mu values for each row of the second order grid
# We will use a as the upper bound for b
# and we will use seq to generate b_size values from 0.1 to a
for (i in 1:nrow(first_order_grid)) {
  row <- first_order_grid[i, ]
  n <- row$n
  p <- row$p
  a <- row$a
  relevant_rows <- (second_order_grid$n == n) & (second_order_grid$p == p) & (second_order_grid$a == a)
  b <- seq(from = 0.1, to = a, length.out = b_size)
  imbalance <- 1 - b / a
  second_order_grid[relevant_rows, "b"]  <- b
  second_order_grid[relevant_rows, "imbalance"]  <- imbalance
}

full_grid <- second_order_grid[rep(seq_len(nrow(second_order_grid)), each = 3), ]
full_grid$mc <- rep(seq_len(3), times = nrow(second_order_grid))

write.csv(full_grid, "grids/propensity_grid.csv", row.names = FALSE)