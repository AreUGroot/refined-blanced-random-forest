#' Train the data with random forest-based algorithms
#'
#' @param train_data Training data in the 2d plane.
#' @param test_data Testing data.
#' @param method Five algorithms are included. "rf": random forest, "wrf": weighted random forest, "brf": balanced random forest, "brf1": balanced random forest with refined weights using method 1, "brf2": balanced random forest with refined weights using method 2.
#'
#' @return Return a list including `model`, the model after trianing, and `measure`, the six measures on the testing data.
#' @export
fit_random_forest <- function(train_data, test_data, method) {
  if (method == "rf") {
    # Random Forest
    rf_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), keep.forest = TRUE)
    measure_rf <- get_print_results(rf_model)
    results <- list(model = rf_model, measure = measure_rf)
  } else if (method == "wrf") {
    # Weighted Random Forest
    class_weights <- c(1e-2, 1e4)
    rf_weighted_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), classwt = class_weights, keep.forest = TRUE)
    rf_weighted_model$test$confusion
    measure_weighted <- get_print_results(rf_weighted_model)
    results <- list(model = rf_weighted_model, measure = measure_weighted)
  } else if (method == "brf") {
    # Balanced random forest
    sample_size <- min(n_train_one, n_train_zero)
    rf_balanced_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), sampsize = c(sample_size, sample_size), keep.forest = TRUE)
    measure_balanced <- get_print_results(rf_balanced_model)
    results <- list(model = rf_balanced_model, measure = measure_balanced)
  } else if (method == "brf1") {
    # Modified Balanced Random Forest with weight 1
    indices_one <- which(train_data$labels == 1)
    dist_from_one <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_one) ^ 2)))
    dist_from_zero <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_zero) ^ 2)))
    dist_zero_from_one <- dist_from_one[-indices_one]
    dist_one_from_zero <- dist_from_zero[indices_one]
    dist_one_from_zero <- apply(as.matrix(train_data[indices_one, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_zero) ^ 2)))
    quantile_percentage <- 0.2
    dist_threshold_one <- quantile(dist_one_from_zero, probs = quantile_percentage)
    dist_threshold_zero <- quantile(dist_zero_from_one, probs = quantile_percentage)
    data_weights <- rep(1, n_train_one + n_train_zero)

    data_weights[(dist_from_one < dist_threshold_zero) & (train_data[, 3] == 0)] <- quantile_percentage / (1 - quantile_percentage)
    data_weights[(dist_from_zero < dist_threshold_one) & (train_data[, 3] == 1)] <- quantile_percentage / (1 - quantile_percentage)
    # ------------ END -----------
    sample_size <- round(n_train_one / 2)
    rf_modified_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), weights = data_weights, mtry = 2, replace = TRUE, keep.forest = TRUE, sampsize = c(sample_size, sample_size), ntree = 300)
    measure_modified <- get_print_results(rf_modified_model)
    results <- list(model = rf_modified_model, measure = measure_modified)
  } else if (method == "brf2") {
    # Modified Balanced Random Forest with weight B
    # ------------------- Method 2 ------------
    indices_one <- which(train_data$labels == 1)
    dist_threshold_zero_for_zero <- 1.4
    dist_threshold_one_for_zero <- 1.5
    dist_threshold_zero_for_one <- 1.6
    dist_threshold_one_for_one <- 1.4
    dist_threshold_zerofar_for_zero <- 1.1
    dist_from_one <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_one) ^ 2)))
    dist_from_zero <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_zero) ^ 2)))

    both_close_indices_for_zero <- (dist_from_zero < dist_threshold_zero_for_zero) & (dist_from_one < dist_threshold_one_for_zero)
    both_close_indices_for_one <- (dist_from_zero < dist_threshold_zero_for_one) & (dist_from_one < dist_threshold_one_for_one)
    close_one_far_zero_indices <- (dist_from_zero > dist_threshold_zerofar_for_zero) & (dist_from_one < dist_threshold_one_for_zero)
    n_high_weight_one <- sum(both_close_indices_for_one[indices_one])
    n_zero_both_close <- sum(both_close_indices_for_zero[-indices_one])
    n_zero_close_one_far_zero <- sum(close_one_far_zero_indices[-indices_one])
    n_zero_remaining <- n_train_zero - n_zero_both_close - n_zero_close_one_far_zero

    weight_zero_close_one_far_zero <- 0.3
    weight_zero_both_close <- (n_zero_remaining - n_zero_close_one_far_zero * weight_zero_close_one_far_zero) / n_zero_both_close
    data_weights <- rep(1, n_train_one + n_train_zero)
    data_weights[both_close_indices_for_one & (train_data[, 3] == 1)] <- (n_train_one - n_high_weight_one) / n_high_weight_one
    data_weights[both_close_indices_for_zero & (train_data[, 3] == 0)] <- weight_zero_both_close
    data_weights[close_one_far_zero_indices & (train_data[, 3] == 0)] <- weight_zero_close_one_far_zero
    sample_size <- round(n_train_one / 2)
    rf_modified_model_two <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), weights = data_weights, mtry = 2, replace = TRUE, keep.forest = TRUE, sampsize = c(sample_size, sample_size), ntree = 300)
    measure_modified_two <- get_print_results(rf_modified_model_two)
    results <- list(model = rf_modified_model_two, measure = measure_modified_two)
  }
  return(results)
}

#' Calibrate the random forest model with prior information and test it on the testing data.
#'
#' @param rf_model The model after training with the ordinary random forest.
#' @param class_zero_ratio The value of $P(Y=0)$, which represents the ratio of data with label 0 among all the data.
#' @param test_data Testing data.
#'
#' @return This function returns six measures of the calibrated model with prior information on the testing data.
#' @export
modify_rf_with_prior <- function(rf_model, class_zero_ratio, test_data) {
  # Random Forest with Prior
  prior_zero <- class_zero_ratio
  prior_ratio <- prior_zero / (1 - prior_zero)
  condi_probs <- rf_model$test$votes
  correct_predicts <- as.factor(cbind((condi_probs[, 1] / prior_ratio) < condi_probs[, 2]))
  confusion_prior <- table(test_data[, 3], correct_predicts)
  measure_prior <- get_six_measures(confusion_prior)
  return(measure_prior)
}

# ----------------------------------------------
get_six_measures <- function(confusion_matrix){
  true_negative <- confusion_matrix[1, 1]
  false_positive <- confusion_matrix[1, 2]
  false_negative <- confusion_matrix[2, 1]
  true_positive <- confusion_matrix[2, 2]
  beta <- 0.5

  acc_minus <- true_negative / (true_negative + false_positive)
  acc_plus <- true_positive / (true_positive + false_negative)
  g_mean <- sqrt(acc_minus * acc_plus)
  weight_acc <- beta * acc_plus + (1 - beta) * acc_minus
  precision <- true_positive / (true_positive + false_positive)
  f_measure <- 2 * precision * acc_plus / (precision + acc_plus)

  results <- c(acc_minus, acc_plus, precision, f_measure, g_mean, weight_acc)
  names(results) <- c("Major accuracy", "Minor accuracy", "Precision", "F-measure", "G-mean", "Weighted accuracy")
  return(results)
}

#' Compute six measures after training
#'
#' @param rf_model The trained model.
#'
#' @return Return six measures: the classiciation accuracy on the major group, the classiciation accuracy on the minor group, precision, F-measure, G-mean, and weighted accuracy.
#' @export
get_print_results <- function(rf_model) {
  confusion_matrix <- rf_model$test$confusion
  results <- get_six_measures(confusion_matrix)
  return(results)
}

# -------------------------------------------------------
#' Train the clustered data with random forest-based algorithms
#'
#' @param train_data Training data in the 2d plane.
#' @param test_data Testing data.
#' @param method Five algorithms are included. "rf": random forest, "wrf": weighted random forest, "brf": balanced random forest, "brf1": balanced random forest with refined weights using method 1, "brf2": balanced random forest with refined weights using method 2.
#'
#' @return Return a list including `model`, the model after trianing, and `measure`, the six measures on the testing data.
#' @export
fit_random_forest_clustered <- function(train_data, test_data, method) {
  if (method == "rf") {
    # Random Forest
    rf_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), keep.forest = TRUE)
    measure_rf <- get_print_results(rf_model)
    results <- list(model = rf_model, measure = measure_rf)
  } else if (method == "wrf") {
    # Weighted Random Forest
    class_weights <- c(1e-2, 1e4)
    rf_weighted_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), classwt = class_weights, keep.forest = TRUE)
    rf_weighted_model$test$confusion
    measure_weighted <- get_print_results(rf_weighted_model)
    results <- list(model = rf_weighted_model, measure = measure_weighted)
  } else if (method == "brf") {
    # Balanced random forest
    sample_size <- min(n_train_one, n_train_zero)
    rf_balanced_model <- randomForest::randomForest(factor(labels) ~ ., data = train_data[, 1:3], xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), sampsize = c(sample_size, sample_size), keep.forest = TRUE)
    measure_balanced <- get_print_results(rf_balanced_model)
    results <- list(model = rf_balanced_model, measure = measure_balanced)
  } else if (method == "brf1") {
    # Modified Balanced Random Forest: A
    n_train_all <- dim(train_data)[1]
    sample_size <- min(n_train_one, n_train_zero, n_train_two)

    indices_major <- which(train_data$labels == 0)
    dist_from_zero <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_zero) ^ 2)))
    dist_from_one <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_one) ^ 2)))
    dist_from_two <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_two) ^ 2)))

    dist_from_minority <- pmin(dist_from_one, dist_from_two)
    dist_zero_from_minority <- dist_from_minority[indices_major]
    dist_minority_from_zero <- dist_from_zero[-indices_major]

    quantile_percentage <- 0.2
    dist_threshold_minority <- quantile(dist_minority_from_zero, probs = quantile_percentage)
    dist_threshold_zero <- quantile(dist_zero_from_minority, probs = quantile_percentage)


    data_weights <- rep(1, n_train_all)
    data_weights[(dist_from_minority < dist_threshold_zero) & (train_data[, 3] == 0)] <- quantile_percentage / (1 - quantile_percentage)
    data_weights[(dist_from_zero < dist_threshold_minority) & train_data[, 3] == 1] <- quantile_percentage / (1 - quantile_percentage) / 3

    sample_size <- (n_train_all - n_train_zero) / 2
    rf_modified_model <- randomForest::randomForest(factor(labels) ~ V1 + V2, data = train_data, xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), weights = data_weights, mtry = 2, replace = TRUE, keep.forest = TRUE, sampsize = c(sample_size, sample_size))
    measure_modified <- get_print_results(rf_modified_model)

    results <- list(model = rf_modified_model, measure = measure_modified)
  } else if (method == "brf2") {
    n_train_all <- dim(train_data)[1]
    # Modified Balanced Random Forest: B
    sample_size <- min(n_train_one, n_train_zero, n_train_two)
    indices_major <- which(train_data$labels == 0)
    is_major <- train_data$labels == 0
    dist_from_zero <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_zero) ^ 2)))
    dist_from_one <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_one) ^ 2)))
    dist_from_two <- apply(as.matrix(train_data[, 1:2]), 1, function(x) sqrt(sum((x - parameters$mean_two) ^ 2)))
    dist_from_minority <- pmin(dist_from_one, dist_from_two)
    quantile_percentage_same <- 0.3
    quantile_percentage_diff <- 0.5
    majority_close_to_minority_threshold <- quantile(dist_from_minority[is_major], probs = quantile_percentage_diff)
    majority_close_to_majority_threshold_far <- quantile(dist_from_zero[is_major], probs = 0.8)
    majority_close_to_majority_threshold_near <- quantile(dist_from_zero[is_major], probs = quantile_percentage_same)
    minority_close_to_minority_threshold <- quantile(dist_from_minority[!is_major], probs = quantile_percentage_same)
    minority_close_to_majority_threshold <- quantile(dist_from_zero[!is_major], probs = quantile_percentage_diff)

    data_weights <- rep(1, n_train_all)
    # Assign high weights for minority data points which are close to both minority and majority centers
    tmp <- (!is_major) & (dist_from_zero < minority_close_to_majority_threshold) & (dist_from_minority < minority_close_to_minority_threshold)
    n_tmp <- sum(tmp)
    data_weights[tmp] <- (n_train_all - n_train_zero) / n_tmp
    # Assign low weights for majority data points which are far from majority center but close to minority center
    tmp1 <- (is_major) & (dist_from_zero > majority_close_to_majority_threshold_far) & (dist_from_minority < majority_close_to_minority_threshold)
    n_tmp1 <- sum(tmp1)
    data_weights[tmp1] <- 1
    # Assign high weights for majority data points which are close to both minority and majority centers
    tmp2 <- (is_major) & (dist_from_zero < majority_close_to_majority_threshold_near) & (dist_from_minority < majority_close_to_minority_threshold)
    n_tmp2 <- sum(tmp2)
    data_weights[tmp2] <- n_train_one / n_tmp2 * 7
    sample_size <- (n_train_all - n_train_zero) / 2
    rf_modified_model_two <- randomForest::randomForest(factor(labels) ~ V1 + V2, data = train_data, xtest = test_data[, 1:2], ytest = factor(test_data[, 3]), weights = data_weights, mtry = 2, replace = TRUE, keep.forest = TRUE, sampsize = c(sample_size, sample_size))
    measure_modified_two <- get_print_results(rf_modified_model_two)
    results <- list(model = rf_modified_model_two, measure = measure_modified_two)
  }
}

