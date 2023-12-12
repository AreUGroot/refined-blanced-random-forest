plot_points <- function(data_all) {
  ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::stat_ellipse(data = data_all, ggplot2::aes(x = V1, y = V2, group = factor(labels), color = factor(labels)), size = 1, level = 0.9) +
    ggplot2::geom_point(data = data_all, ggplot2::aes(x = V1, y = V2, group = factor(labels), color = factor(labels)), shape = 1) +
    ggplot2::scale_color_manual(values = c("red", "deepskyblue")) +
    ggplot2::ggtitle("Sampled Data")
}

# Draw plots

draw_boundary_points <- function(rf_model, data_all, my_title) {
  length_out <- 200
  x1_range <- seq(min(data_all[, 1]), max(data_all[, 1]), length.out = length_out)
  x2_range <- seq(min(data_all[, 2]), max(data_all[, 2]), length.out = length_out)
  grid <- expand.grid(V1 = x1_range, V2 = x2_range)
  predict_grids <- predict(rf_model, newdata = grid)
  grid$Class <- predict_grids

  this_plot <- ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::geom_tile(data = grid, ggplot2::aes(x = V1, y = V2, fill = Class), alpha = 0.5) +
    ggplot2::scale_fill_manual(values = c("white", "deepskyblue")) +
    ggplot2::stat_ellipse(data = data_all, ggplot2::aes(x = V1, y = V2, group = factor(labels)), linetype = "dashed", size = 1, level = 0.9) +
    ggplot2::ggtitle(my_title)
  return(this_plot)
}


draw_boundary_points_prior <- function(rf_model, data_all, my_title, prior_ratio) {
  length_out <- 200
  x1_range <- seq(min(data_all[, 1]), max(data_all[, 1]), length.out = length_out)
  x2_range <- seq(min(data_all[, 2]), max(data_all[, 2]), length.out = length_out)
  grid <- expand.grid(V1 = x1_range, V2 = x2_range)
  condi_probs <- predict(rf_model, newdata = grid, type = "prob")
  correct_predicts <- as.numeric(cbind((condi_probs[, 1] / prior_ratio) < condi_probs[, 2]))
  grid$Class <- as.factor(correct_predicts)

  this_plot <- ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::geom_tile(data = grid, ggplot2::aes(x = V1, y = V2, fill = Class), alpha = 0.5) +
    ggplot2::scale_fill_manual(values = c("white", "deepskyblue")) +
    ggplot2::stat_ellipse(data = data_all, ggplot2::aes(x = V1, y = V2, group = factor(labels)), linetype = "dashed", size = 1, level = 0.9) +
    ggplot2::ggtitle(my_title)
  return(this_plot)
}

draw_six <- function(data_all, rf_model, rf_weighted_model, rf_balanced_model, rf_data_model, rf_modified_model_two, class_zero_ratio) {
  prior_zero <- class_zero_ratio
  prior_ratio <- prior_zero / (1 - prior_zero)
  plot_one <- draw_boundary_points(rf_model, data_all, "Random Forest")
  plot_two <- draw_boundary_points_prior(rf_model, data_all, "Random Forest with Prior", prior_ratio)
  plot_three <- draw_boundary_points(rf_weighted_model, data_all, "Weigted Random Forest")
  plot_four <- draw_boundary_points(rf_balanced_model, data_all, "Balanced Random Forest")
  plot_five <- draw_boundary_points(rf_data_model, data_all, "Modifed Balanced Random Forest")
  plot_six <- draw_boundary_points(rf_modified_model_two, data_all, "Modifed Balanced Random Forest II")
  gridExtra::grid.arrange(plot_one, plot_two, plot_three, plot_four, plot_five, plot_six, nrow = 2, ncol = 3, top = "Overall Title")
}


draw_three <- function(data_all, rf_balanced_model, rf_data_model, rf_modified_model_two) {
  plot_four <- draw_boundary_points(rf_balanced_model, data_all, "Balanced Random Forest")
  plot_five <- draw_boundary_points(rf_data_model, data_all, "Modifed Balanced Random Forest")
  plot_six <- draw_boundary_points(rf_modified_model_two, data_all, "Modifed Balanced Random Forest II")
  gridExtra::grid.arrange(plot_four, plot_five, plot_six, nrow = 3, ncol = 1, top = "Overall Title")
}

# ----------------------------------------------------------

draw_boundary_points_clustered <- function(rf_model, data_all, my_title) {
  length_out <- 200
  x1_range <- seq(min(data_all[, 1]), max(data_all[, 1]), length.out = length_out)
  x2_range <- seq(min(data_all[, 2]), max(data_all[, 2]), length.out = length_out)
  grid <- expand.grid(V1 = x1_range, V2 = x2_range)
  predict_grids <- predict(rf_model, newdata = grid)
  grid$Class <- predict_grids

  this_plot <- ggplot2::ggplot() +
   ggplot2::coord_fixed(ratio = 1) +
   ggplot2::geom_tile(data = grid, ggplot2::aes(x = V1, y = V2, fill = Class), alpha = 0.5) +
   ggplot2::scale_fill_manual(values = c("white", "deepskyblue")) +
   ggplot2::stat_ellipse(data = data_all, ggplot2::aes(x = V1, y = V2, group = factor(cluster)), linetype = "dashed", size = 1, level = 0.7) +
   ggplot2::ggtitle(my_title)
  return(this_plot)
}


draw_boundary_points_prior_clustered <- function(rf_model, data_all, my_title, prior_ratio) {
  length_out <- 200
  x1_range <- seq(min(data_all[, 1]), max(data_all[, 1]), length.out = length_out)
  x2_range <- seq(min(data_all[, 2]), max(data_all[, 2]), length.out = length_out)
  grid <- expand.grid(V1 = x1_range, V2 = x2_range)
  condi_probs <- predict(rf_model, newdata = grid, type = "prob")
  correct_predicts <- as.numeric(cbind((condi_probs[, 1] / prior_ratio) < condi_probs[, 2]))
  grid$Class <- as.factor(correct_predicts)

  this_plot <- ggplot2::ggplot() +
   ggplot2::coord_fixed(ratio = 1) +
   ggplot2::geom_tile(data = grid, ggplot2::aes(x = V1, y = V2, fill = Class), alpha = 0.5) +
   ggplot2::scale_fill_manual(values = c("white", "deepskyblue")) +
   ggplot2::stat_ellipse(data = data_all, ggplot2::aes(x = V1, y = V2, group = factor(cluster)), linetype = "dashed", size = 1, level = 0.7) +
   ggplot2::ggtitle(my_title)
  return(this_plot)
}

draw_six_clustered <- function(data_all, rf_model, rf_weighted_model, rf_balanced_model, rf_data_model, rf_modified_model_two, prior_ratio) {
  plot_one <- draw_boundary_points_clustered(rf_model, data_all, "Random Forest")
  plot_two <- draw_boundary_points_prior_clustered(rf_model, data_all, "Random Forest with Prior", prior_ratio)
  plot_three <- draw_boundary_points_clustered(rf_weighted_model, data_all, "Weigted Random Forest")
  plot_four <- draw_boundary_points_clustered(rf_balanced_model, data_all, "Balanced Random Forest")
  plot_five <- draw_boundary_points_clustered(rf_data_model, data_all, "Modifed Balanced Random Forest")
  plot_six <- draw_boundary_points_clustered(rf_modified_model_two, data_all, "Modifed Balanced Random Forest II")
  gridExtra::grid.arrange(plot_one, plot_two, plot_three, plot_four, plot_five, plot_six, nrow = 2, ncol = 3, top = "Overall Title")
}


draw_three_clustered <- function(data_all, rf_balanced_model, rf_data_model, rf_modified_model_two) {
  plot_four <- draw_boundary_points_clustered(rf_balanced_model, data_all, "Balanced Random Forest")
  plot_five <- draw_boundary_points_clustered(rf_data_model, data_all, "Modifed Balanced Random Forest")
  plot_six <- draw_boundary_points_clustered(rf_modified_model_two, data_all, "Modifed Balanced Random Forest II")
  gridExtra::grid.arrange(plot_four, plot_five, plot_six, nrow = 1, ncol = 3, top = "Overall Title")
}


plot_clustered_points <- function(train_data, n_train_zero, n_train_one, n_train_two) {
  ggplot2::ggplot() +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::stat_ellipse(data = train_data, ggplot2::aes(x = V1, y = V2, group = factor(cluster), color = factor(labels)), size = 1, level = 0.9) +
    ggplot2::geom_point(data = train_data, ggplot2::aes(x = V1, y = V2), color = "red", shape = 1) +
    ggplot2::geom_point(data = train_data[n_train_zero + (1:n_train_one), ], ggplot2::aes(x = V1, y = V2), color = "deepskyblue", shape = 1) +
    ggplot2::geom_point(data = train_data[n_train_zero + n_train_one + (1:n_train_two), ], ggplot2::aes(x = V1, y = V2), color = "deepskyblue", shape = 1) +
    # geom_point(data = data_tmp_zero, aes(x = V1, y = V2, size = factor(data_weights_tmp_zero)), color = "red", ) +
    # scale_size_manual(values = c(1, 1, 3)) +
    # scale_color_manual(values = c("red", "deepskyblue")) +
    ggplot2::ggtitle("Sampled Data")
}

