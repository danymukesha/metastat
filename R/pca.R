#' PCA Score and Loadings Plot
#'
#' Creates a PCA scores and loadings plot using ggplot2 and FactoMineR.
#'
#' @name pca
#'
#' @param X An array-like object that contains the input data.
#' @param pcx A positive integer representing the principal component for the x-axis (default: 1).
#' @param pcy A positive integer representing the principal component for the y-axis (default: 2).
#' @param group_label A vector specifying group labels for coloring points in the score plot (default: NULL).
#' @param sample_label A vector specifying labels for individual points in the score plot (default: NULL).
#' @param peak_label A vector specifying labels for individual points in the loadings plot (default: NULL).
#' @param plot_ci A logical indicating whether to plot 95% confidence ellipses in the score plot (default: TRUE).
#' @param grid_line A logical indicating whether to show grid lines in both plots (default: TRUE).
#'
#' @import ggplot2
#' @import FactoMineR
#' @import ggfortify
#' @importFrom stats as.data.frame
#'
#' @examples
#' X <- matrix(rnorm(100), ncol = 10)
#' pca_plot(X, pcx = 1, pcy = 2, group_label = rep(1:5, each = 20), sample_label = rep(1:10, each = 10), plot_ci = TRUE, grid_line = TRUE)
#'
#' @export


library(ggplot2)
library(FactoMineR)
library(ggfortify)

pca_plot <- function(X, pcx = 1, pcy = 2, group_label = NULL, sample_label = NULL, peak_label = NULL, plot_ci = TRUE, grid_line = TRUE) {
  # Set model
  model <- PCA(X, graph = FALSE)

  # Extract scores, explained variance, and loadings for pcx and pcy
  scores <- as.data.frame(model$ind$coord[, c(pcx, pcy)])
  loadings <- as.data.frame(model$var$coord[, c(pcx, pcy)])
  explained_var <- model$var$contrib[, c(pcx, pcy)]

  # Combine group_label and sample_label if provided
  group_sample_label <- ifelse(!is.null(sample_label), paste(group_label, sample_label, sep = "_"), as.character(group_label))

  # Scores plot
  fig_score <- ggplot(scores, aes_string(x = colnames(scores)[1], y = colnames(scores)[2], color = group_sample_label)) +
    geom_point(size = 5) +
    labs(x = sprintf("PC %d (%.1f%%)", pcx, explained_var[1]), y = sprintf("PC %d (%.1f%%)", pcy, explained_var[2]),
         title = sprintf("PCA Score Plot (PC%d vs. PC%d)", pcx, pcy),
         caption = "PC: Principal Component") +
    theme_minimal() +
    theme(legend.position = "bottom", text = element_text(size = 15))

  # Loadings plot
  fig_load <- ggplot(loadings, aes_string(x = colnames(loadings)[1], y = colnames(loadings)[2])) +
    geom_point(size = 7, shape = 24, fill = "white", color = "black") +
    labs(x = sprintf("PC %d (%.1f%%)", pcx, explained_var[1]), y = sprintf("PC %d (%.1f%%)", pcy, explained_var[2]),
         title = sprintf("PCA Loadings Plot (PC%d vs. PC%d)", pcx, pcy),
         caption = "PC: Principal Component") +
    theme_minimal() +
    theme(legend.position = "none", text = element_text(size = 15)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray")

  # Score plot extra: 95% confidence ellipse using PCA
  if (plot_ci) {
    ellipses <- PCAellipse(model, axes = c(pcx, pcy), level = 0.95, contrib = "cos2")
    fig_score <- fig_score + geom_path(data = ellipses$ind$coord, aes(x = Dim.1, y = Dim.2, group = group_sample_label), color = "gray", linetype = "dashed", size = 1)
  }

  # Remove grid lines
  if (!grid_line) {
    fig_score <- fig_score + theme(panel.grid = element_blank())
    fig_load <- fig_load + theme(panel.grid = element_blank())
  }

  # Print the plots
  print(fig_score)
  print(fig_load)
}

# Example usage:
# X <- matrix(rnorm(100), ncol = 10)
# pca_plot(X, pcx = 1, pcy = 2, group_label = rep(1:5, each = 20), sample_label = rep(1:10, each = 10), plot_ci = TRUE, grid_line = TRUE)
