integrand <- function(x, mu, sigma, nt, nc) {
  term1 <- dnorm(x)
  term2 <- pnorm(x)^(nt - 1)
  term3 <- (1 - pnorm((x - mu) / sigma))^nc
  nt * term1 * term2 * term3
}

prob_overlap <- function(mu, sigma, n, p) {
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
  1 - result$value
}

mu_upper <- function(n, p, sd) {
  x <- 0
  step_size <- 0.1
  prob_overlap <- prob_overlap(x, sd, n, p)
  if (prob_overlap < 0.99) {
    return(NA)
  }
  while (prob_overlap > 0.99) {
    x <- x + step_size
    prob_overlap <- prob_overlap(x, sd, n, p)
  }
  x
}

create_extended_grid <- function(
  first_order_grid,
  new_col_name,
  max_value_fn,
  grid_size
) {
  # Create the expanded grid by repeating the first order grid
  extended_grid <- first_order_grid
  for (i in 1:(grid_size - 1)) {
    extended_grid <- rbind(extended_grid, first_order_grid)
  }
  extended_grid[[new_col_name]] <- NA
  extended_grid[["imbalance"]] <- NA
  
  # Fill in values for the new column
  for (i in 1:nrow(first_order_grid)) {
    row <- first_order_grid[i, ]
    # Create a matching condition based on all columns in first_order_grid
    match_conditions <- sapply(names(first_order_grid), function(col) {
      extended_grid[[col]] == row[[col]]
    })
    relevant_rows <- Reduce(`&`, match_conditions)
    max_val <- do.call(max_value_fn, as.list(row))
    if (!is.na(max_val)) {
      values <- seq(from = 0, to = max_val, length.out = grid_size)
      imbalance <- values/max_val
      extended_grid[relevant_rows, new_col_name] <- values
      extended_grid[relevant_rows, "imbalance"] <- imbalance
    }
  }
  # Clean up zero values and NAs if needed
  extended_grid <- extended_grid[!is.na(extended_grid[[new_col_name]]), ]
  extended_grid <- extended_grid[extended_grid[[new_col_name]] != 0, ]
  extended_grid
}