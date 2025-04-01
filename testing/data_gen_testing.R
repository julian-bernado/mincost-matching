source("scripts/data_gen.R")

library(tidyverse)

### Test case 1: 
### pure noise, 500 observations, 2 covariates, same covariate distributions for 
### treated and control
n <- 500
prob_treated <- 0.5
beta <- rep(0, 2)
tau <- 0
beta_tau <- rep(0, 2)
sigma <- 1
bounded_params <- list(
  data.frame(a = 1, b = 1), data.frame(a = 1, b = 1)
)
cont_params <- list(
  data.frame(mean = 0, sd = 1), data.frame(mean = 0, sd = 1)
)
test1_data <- gen_data(
  n, prob_treated, beta, tau, beta_tau, sigma, bounded_params, cont_params
)

# Check proportion of treated
prop.table(table(test1_data$Z))

# Check the distribution of the bounded covariates by treatment status
test1_data |> ggplot(aes(x = bounded_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the continuous covariates by treatment status
test1_data |> ggplot(aes(x = cont_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the outcome by treatment status
test1_data |> ggplot(aes(x = Y, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

### Test case 2:
### pure noise, 500 observations, 2 covariates, different covariate
### distributions for treated and control
n <- 500
prob_treated <- 0.5
beta <- c(0, 0)
tau <- 0
beta_tau <- c(0, 0)
sigma <- 1
bounded_params <- list(
  data.frame(a = 1, b = 1), data.frame(a = 10, b = 10)
)
cont_params <- list(
  data.frame(mean = 0, sd = 1), data.frame(mean = 10, sd = 1)
)
test2_data <- gen_data(
  n, prob_treated, beta, tau, beta_tau, sigma, bounded_params, cont_params
)

# Check proportion of treated
prop.table(table(test2_data$Z))

# Check the distribution of the bounded covariates by treatment status
test2_data |> ggplot(aes(x = bounded_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the continuous covariates by treatment status
test2_data |> ggplot(aes(x = cont_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the outcome by treatment status
test2_data |> ggplot(aes(x = Y, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

### Test case 3:
### non-heterogeneous treatment effect, 500 observations, 2 covariates, same 
### covariate distributions for treated and control.
n <- 500
prob_treated <- 0.5
beta <- c(0, 0)
tau <- 10
beta_tau <- c(0, 0)
sigma <- 1
bounded_params <- list(
  data.frame(a = 1, b = 1), data.frame(a = 1, b = 1)
)
cont_params <- list(
  data.frame(mean = 0, sd = 1), data.frame(mean = 0, sd = 1)
)
test3_data <- gen_data(
  n, prob_treated, beta, tau, beta_tau, sigma, bounded_params, cont_params
)

# Check proportion of treated
prop.table(table(test3_data$Z))

# Check the distribution of the bounded covariates by treatment status
test3_data |> ggplot(aes(x = bounded_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the continuous covariates by treatment status
test3_data |> ggplot(aes(x = cont_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the outcome by treatment status
test3_data |> ggplot(aes(x = Y, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

### Test case 4:
### covariate effects, no treatment effect, 500 observations, 2 covariates, 
### different covariate distributions for treated and control.
n <- 500
prob_treated <- 0.5
beta <- c(5, 10)
tau <- 0
beta_tau <- c(0, 0)
sigma <- 1
bounded_params <- list(
  data.frame(a = 1, b = 1), data.frame(a = 10, b = 10)
)
cont_params <- list(
  data.frame(mean = 0, sd = 1), data.frame(mean = 10, sd = 1)
)
test4_data <- gen_data(
  n, prob_treated, beta, tau, beta_tau, sigma, bounded_params, cont_params
)

# Check proportion of treated
prop.table(table(test4_data$Z))

# Check the distribution of the bounded covariates by treatment status
test4_data |> ggplot(aes(x = bounded_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the continuous covariates by treatment status
test4_data |> ggplot(aes(x = cont_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the outcome by treatment status
test4_data |> ggplot(aes(x = Y, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

### Test case 5:
### covariate and heterogeneous treatment effects, 500 observations, 2 
### covariates, different covariate distributions for treated and control.
n <- 500
prob_treated <- 0.5
beta <- c(5, 10)
tau <- 5
beta_tau <- c(5, 10)
sigma <- 1
bounded_params <- list(
  data.frame(a = 1, b = 1), data.frame(a = 10, b = 10)
)
cont_params <- list(
  data.frame(mean = 0, sd = 1), data.frame(mean = 10, sd = 1)
)
test5_data <- gen_data(
  n, prob_treated, beta, tau, beta_tau, sigma, bounded_params, cont_params
)

# Check proportion of treated
prop.table(table(test5_data$Z))

# Check the distribution of the bounded covariates by treatment status
test5_data |> ggplot(aes(x = bounded_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the continuous covariates by treatment status
test5_data |> ggplot(aes(x = cont_1, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)

# Check the distribution of the outcome by treatment status
test5_data |> ggplot(aes(x = Y, fill = factor(Z))) +
  geom_histogram(alpha = 0.5) +
  facet_wrap(~ Z)
