library(dplyr)
source("scripts/grid_helpers.R")

# Shared parameters
n_lower <- 10
n_lower_exp <- log10(n_lower)
n_upper <- 1e3
n_upper_exp <- log10(n_upper)
n_size <- 10
n <- round(10^seq(from = n_lower_exp, to = n_upper_exp, length.out = n_size))

p_lower <- 0.05
p_upper <- 0.95
p_size <- 5
p <- seq(from = p_lower, to = p_upper, length.out = p_size)

mc_lower <- 1
mc_size <- 5

# Prognostic grid parameters
sd_lower <- 1/5
sd_upper <- 5
sd_size <- 5
sd <- round(seq(from = sd_lower, to = sd_upper, length.out = sd_size), 2)

mu_size <- 5

# Propensity grid parameters
a_lower <- 1
a_upper <- 50
a_size <- 5
a <- seq(from = a_lower, to = a_upper, length.out = a_size)

b_size <- 5

# Generate prognostic grid
prog_first_order <- expand.grid(
  n = n,
  p = p,
  sd = sd
)

prog_second_order <- prog_first_order
for (i in 1:(mu_size - 1)){
  prog_second_order <- rbind(prog_second_order, prog_first_order)
}
prog_second_order$mu <- NA
prog_second_order$imbalance <- NA

for (i in 1:nrow(prog_first_order)) {
  row <- prog_first_order[i, ]
  cur_n <- row$n
  cur_p <- row$p
  cur_sd <- row$sd
  relevant_rows <- (prog_second_order$n == cur_n) & (prog_second_order$p == cur_p) & (prog_second_order$sd == cur_sd)
  max_mu <- mu_upper(cur_n, cur_p, cur_sd)
  if (!is.na(max_mu)) {
    mu <- seq(from = 0, to = max_mu, length.out = mu_size)
    imbalance <- mu/max_mu
    prog_second_order[relevant_rows, "mu"]  <- mu
    prog_second_order[relevant_rows, "imbalance"]  <- imbalance
  }
}
prog_second_order <- prog_second_order[!is.na(prog_second_order$mu), ]
prog_second_order <- prog_second_order[prog_second_order$mu != 0, ]

# Generate propensity grid
prop_first_order <- expand.grid(
  n = n,
  p = p,
  a = a
)

prop_second_order <- prop_first_order
for (i in 1:(b_size - 1)){
  prop_second_order <- rbind(prop_second_order, prop_first_order)
}
prop_second_order$b <- NA
prop_second_order$imbalance <- NA

for (i in 1:nrow(prop_first_order)) {
  row <- prop_first_order[i, ]
  cur_n <- row$n
  cur_p <- row$p
  cur_a <- row$a
  relevant_rows <- (prop_second_order$n == cur_n) & (prop_second_order$p == cur_p) & (prop_second_order$a == cur_a)
  b <- seq(from = 0.1, to = cur_a, length.out = b_size)
  imbalance <- 1 - b / cur_a
  prop_second_order[relevant_rows, "b"]  <- b
  prop_second_order[relevant_rows, "imbalance"]  <- imbalance
}

# Generate shared mc grid
first_order_mc_grid <- expand.grid(
  n = n,
  p = p
)

second_order_mc_grid <- first_order_mc_grid
for (i in 1:(mc_size - 1)){
  second_order_mc_grid <- rbind(second_order_mc_grid, first_order_mc_grid)
}
second_order_mc_grid$mc <- NA

for (i in 1:nrow(first_order_mc_grid)) {
  row <- first_order_mc_grid[i, ]
  cur_n <- row$n
  cur_p <- row$p
  relevant_rows <- (second_order_mc_grid$n == cur_n) & (second_order_mc_grid$p == cur_p)
  max_mc <- max((1 / 2) * (1 - cur_p) * cur_n, 1)  # Using max with 1 from prognostic version
  mc <- round(seq(from = mc_lower, to = max_mc, length.out = mc_size))
  rel_mc <- mc / max_mc
  second_order_mc_grid[relevant_rows, "mc"]  <- mc
  second_order_mc_grid[relevant_rows, "rel_mc"]  <- rel_mc
}

# Create and save final grids
prog_full_grid <- full_join(prog_second_order, second_order_mc_grid, by = c("n", "p"), relationship = "many-to-many")
prop_full_grid <- full_join(prop_second_order, second_order_mc_grid, by = c("n", "p"), relationship = "many-to-many")

write.csv(prog_full_grid, "grids/prognostic_grid.csv", row.names = FALSE)
write.csv(prop_full_grid, "grids/propensity_grid.csv", row.names = FALSE)
