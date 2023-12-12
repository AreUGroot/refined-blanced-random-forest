create_2dgaussian_data <- function(n_zero, n_one, mean_zero, cov_zero, mean_one, cov_one) {
  class_zero <- MASS::mvrnorm(n = n_zero, mean_zero, cov_zero)
  class_one <- MASS::mvrnorm(n = n_one, mean_one, cov_one)
  labels <- c(rep(0, n_zero), rep(1, n_one))
  all_points <- rbind(class_zero, class_one)
  data_all <- data.frame(cbind(all_points, labels))
  return(data_all)
}


#' Generate data with two groups, each of which comes from a Gaussian distribution respectively.
#'
#' @param parameters A list containing the mean of the major distribution, the covariance of the major distribution, the mean of the minor distribution and the covariance of the minor distribution
#' @param n_train_zero The sample size of the major group.
#' @param n_train_one The sample size of the minor group.
#'
#' @return This function returns a list including `train_data` the training data and `test_data` the testing data
#' @export
generate_data <- function(parameters, n_train_zero, n_train_one) {
  n_test_zero <- 2000
  n_test_one <- 2000
  train_data <- create_2dgaussian_data(n_train_zero, n_train_one, parameters$mean_zero, parameters$cov_zero, parameters$mean_one, parameters$cov_one)
  test_data <- create_2dgaussian_data(n_test_zero, n_test_one, parameters$mean_zero, parameters$cov_zero, parameters$mean_one, parameters$cov_one)
  results <- list(train_data = train_data, test_data = test_data)
}

create_clustered_2dgaussian_data <- function(n_zero, n_one, n_two, mean_zero, cov_zero, mean_one, cov_one, mean_two, cov_two) {
  class_zero <- MASS::mvrnorm(n = n_zero, mean_zero, cov_zero)
  class_one <- MASS::mvrnorm(n = n_one, mean_one, cov_one)
  class_two <- MASS::mvrnorm(n = n_two, mean_two, cov_two)
  labels <- c(rep(0, n_zero), rep(1, n_one), rep(1, n_two))
  cluster <- c(rep(0, n_zero), rep(1, n_one), rep(2, n_two))
  all_points <- rbind(class_zero, class_one, class_two)
  data_all <- data.frame(cbind(all_points, labels, cluster))
  return(data_all)
}


#' Generate data with two groups, for which the major group comes from a Gaussian distribution and the minor group comes from a mixture of two Gaussian distributions.
#'
#' @param parameters A list containing the mean of the major distribution, the covariance of the major distribution, the mean of the minor distribution and the covariance of the minor distribution
#' @param n_train_zero The sample size of the major group.
#' @param n_train_one The sample size of the minor group from the first distribution of the mixture of Gaussian.
#' @param n_train_two The sample size of the minor group from the second distribution of the mixture of Gaussian.
#'
#' @return This function returns a list including `train_data` the training data and `test_data` the testing data
#' @export
generate_clustered_data <- function(parameters, n_train_zero, n_train_one, n_train_two) {
  n_test_zero <- 2000
  n_test_one <- 1000
  n_test_two <- 1000
  train_data <- create_clustered_2dgaussian_data(n_train_zero, n_train_one, n_train_two, parameters$mean_zero, parameters$cov_zero, parameters$mean_one, parameters$cov_one, parameters$mean_two, parameters$cov_two)
  test_data <- create_clustered_2dgaussian_data(n_test_zero, n_test_one, n_test_two, parameters$mean_zero, parameters$cov_zero, parameters$mean_one, parameters$cov_one, parameters$mean_two, parameters$cov_two)
  results <- list(train_data = train_data, test_data = test_data)
}
