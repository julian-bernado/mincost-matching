# This file will take in the results unders `results`
# It will then generate a new column corresponding to fastest algorithm
# Then, it will do a train-test-val split of the dataframe
# It will then tune a few hyperparameters for a random forest model
# After tuning with the test set, we will evaluate the model on the val set
# We will see if the model's predictions are better than the baseline
# The baseline here will just be to always select simplex

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(randomForest)
library(pdp)
set.seed(610)

for(setting in c("prognostic", "propensity")) {
  # Read results
  results <- readRDS(paste0("results/", setting, "_results.rds"))

  # Add fastest algorithm column
  results <- results %>%
    mutate(fastest_algo = case_when(
      simplex_times <= cost_times & simplex_times <= cycle_times & simplex_times <= capacity_times ~ "simplex",
      cost_times <= simplex_times & cost_times <= cycle_times & cost_times <= capacity_times ~ "cost",
      cycle_times <= simplex_times & cycle_times <= cost_times & cycle_times <= capacity_times ~ "cycle",
      TRUE ~ "capacity"
    ))

  # Create features and target
  model_data <- results %>%
    mutate(fastest_algo = factor(fastest_algo))

  # Create train/test/val split (60/20/20)
  n <- nrow(model_data)
  train_idx <- sample(1:n, size = floor(0.6 * n))
  remain_idx <- setdiff(1:n, train_idx)
  test_idx <- sample(remain_idx, size = floor(0.5 * length(remain_idx)))
  val_idx <- setdiff(remain_idx, test_idx)

  train_data <- model_data[train_idx, ]
  test_data <- model_data[test_idx, ]
  val_data <- model_data[val_idx, ]

  # Define tuning grid
  tuning_grid <- expand.grid(
    mtry = c(2, 3, 4, 5),
    ntree = c(50, 100, 500)
  )

  # Train models with different hyperparameters on training data
  # and evaluate on test data
  test_accuracies <- data.frame(
    mtry = integer(),
    ntree = integer(),
    accuracy = numeric()
  )

  for (mtry_val in tuning_grid$mtry) {
    for( ntree_val in tuning_grid$ntree) {
      # Train random forest model
      rf_current <- randomForest(
        fastest_algo ~ n + p + mc + rel_mc + imbalance,
        data = train_data,
        mtry = mtry_val,
        ntree = ntree_val
      )
      
      # Evaluate on test set
      test_pred <- predict(rf_current, test_data)
      test_accuracy <- mean(test_pred == test_data$fastest_algo)
      
      # Store results
      test_accuracies <- rbind(test_accuracies, data.frame(mtry = mtry_val, ntree = ntree_val, accuracy = test_accuracy))
    }
  }

  # Find best hyperparameters
  best_params <- test_accuracies[which.max(test_accuracies$accuracy), ]
  print("best params")
  print(best_params)

  # Train final model with best hyperparameters
  final_model <- randomForest(
    fastest_algo ~ n + p + mc + rel_mc + imbalance,
    data = rbind(train_data, test_data),  # Use both train and test for final model
    mtry = best_params$mtry,
    ntree = best_params$ntree
  )

  # Evaluate on validation set
  val_pred <- predict(final_model, val_data)
  val_data$val_pred <- val_pred

  # Now, let's compare the predictions with the actual time
  baseline_time <- 0
  predictive_time <- 0
  for (i in 1:nrow(val_data)) {
    pred_best <- val_data$val_pred[i]
    baseline_time <- baseline_time + val_data[i, "simplex_times"]
    predictive_time <- predictive_time + val_data[i, paste0(pred_best, "_times")]
  }

  # Print results
  print(paste0("Predictive total time: ", predictive_time))
  print(paste0("Baseline total time: ", baseline_time))

  # Variable importance plot
  final_model$importance

  # Create a partial dependence plot for 'x1' (for the "yes" class)
  pd <- partial(final_model, pred.var = "imbalance", which.class = "simplex")
  colnames(pd) <- c("imbalance", "partial_dependence")

  # Plot the partial dependence with ggplot and save it
  pd_plot <- ggplot(pd, aes(x = imbalance, y = partial_dependence)) +
    geom_line() +
    labs(title = "Partial Dependence Plot for Imbalance", x = "Imbalance", y = "Partial Dependence") +
    theme_minimal()

  ggsave(paste0("figures/plots/", setting, "_pdp_imbalance.png"), plot = pd_plot, width = 8, height = 6)
} 