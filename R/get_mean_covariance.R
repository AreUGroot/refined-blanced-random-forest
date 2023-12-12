#' Get prespecified mean and covariances for the Gaussian distribution from which the data come.
#'
#' @param choice Taking 1, 2, 3, or 4, this function offers four choices to allow different settings of data distribution.
#' @param mean_dist This argument specifies the distance between the mean of two Gaussian distribution.
#'
#' @return This function returns a list including `mean_zero`, `mean_one`, `cov_zero`, `cov_one`
#' @export
get_mean_covariance <- function(choice, mean_dist) {
  if (choice == 1) {
    mean_zero <- c(0, 0)
    mean_one <- c(mean_dist, mean_dist)
    cov_zero <- matrix(c(1, 0, 0, 1), 2, 2)
    cov_one <- matrix(c(1, 0, 0, 1), 2, 2)
  } else if (choice == 2) {
    mean_zero <- c(0, 0)
    mean_one <- c(mean_dist, mean_dist)
    cov_zero <- matrix(c(1, 0.5, 0.5, 1), nrow = 2)
    cov_one <- matrix(c(1, -0.5, -0.5, 1), nrow = 2)
  } else if (choice == 3) {
    mean_zero <- c(mean_dist, 0)
    mean_one <- c(-mean_dist, 0)
    cov_zero <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
    cov_one <- matrix(c(1, 0, 0, 1), nrow = 2)
  } else if (choice == 4) {
    mean_zero <- c(mean_dist * 2, 0)
    mean_one <- c(-mean_dist * 2, 0)
    cov_zero <- matrix(c(1, 0.8, 0.8, 1), nrow = 2)
    cov_one <- matrix(c(1, -0.2, -0.2, 1), nrow = 2)
  }
  return(list(mean_zero = mean_zero, mean_one = mean_one, cov_zero = cov_zero, cov_one = cov_one))
}

#' Get prespecified mean and covariances for the Gaussian distribution from which the data come.
#'
#' @param choice Taking 1, 2, 3, or 4, this function offers four choices to allow different settings of data distribution.
#' @param mean_dist This argument specifies the distance between the mean of two Gaussian distribution.
#'
#' @return This function returns a list including `mean_zero`, `mean_one`, `mean_two`, `cov_zero`, `cov_one`, `cov_two`. Zero corresponds to the major distribution, one and two correspond to two distributions in the mixture of Gaussian.
#' @export
get_mean_covariance_clustered <- function(choice, mean_dist) {
  if (choice == 1) {
    mean_zero <- c(0, 0)
    mean_one <- c(mean_dist, mean_dist)
    mean_two <- c(-mean_dist, -mean_dist)
    cov_zero <- matrix(c(2, 0, 0, 2), 2, 2)
    cov_one <- matrix(c(1, 0, 0, 1), 2, 2)
    cov_two <- matrix(c(1, 0, 0, 1), 2, 2)
  } else if (choice == 2) {
    mean_zero <- c(0, 0)
    mean_one <- c(mean_dist, mean_dist)
    mean_two <- c(-mean_dist, -mean_dist)
    cov_zero <- matrix(c(2, 0, 0, 2), 2, 2)
    cov_one <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
    cov_two <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
  } else if (choice == 3) {
    mean_zero <- c(0, 0)
    mean_one <- c(mean_dist, mean_dist)
    mean_two <- c(-mean_dist, -mean_dist)
    cov_zero <- matrix(c(2, 0, 0, 2), 2, 2)
    cov_one <- matrix(c(1, 0.2, -0.2, 1), 2, 2)
    cov_two <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
  } else if (choice == 4) {
    mean_zero <- c(0, 0)
    mean_one <- c(mean_dist, mean_dist)
    mean_two <- c(-mean_dist, mean_dist)
    cov_zero <- matrix(c(2, 0, 0, 2), 2, 2)
    cov_one <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
    cov_two <- matrix(c(1, -0.2, 0.2, 1), 2, 2)
  }
  return(list(mean_zero = mean_zero, mean_one = mean_one, mean_two = mean_two, cov_zero = cov_zero, cov_one = cov_one, cov_two = cov_two))
}

