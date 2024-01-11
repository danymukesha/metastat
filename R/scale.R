#' Scale a matrix or data frame
#'
#' Scales the input matrix or data frame using various scaling methods.
#'
#' @param x An array-like object that contains the data.
#' @param axis An integer or \code{NULL} (default \code{1}). The axis along which to operate.
#' @param ddof An integer (default \code{1}). The degrees of freedom correction.
#' @param method A string (default \code{"auto"}). Method used to scale \code{x}.
#'   Accepted methods are \code{"auto"}, \code{"pareto"}, \code{"vast"}, \code{"level"}, or \code{"range"}.
#' @param mu A number or \code{"default"} (default \code{"default"}).
#'   If \code{mu} is provided, it is used; otherwise, it is calculated.
#' @param sigma A number or \code{"default"} (default \code{"default"}).
#'   If \code{sigma} is provided, it is used; otherwise, it is calculated.
#' @param return_mu_sigma A logical (default \code{FALSE}).
#'   If \code{TRUE}, \code{mu} and \code{sigma} are returned instead of \code{z}.
#'
#' @return If \code{return_mu_sigma = FALSE}, an array-like object containing the scaled data.
#'   If \code{return_mu_sigma = TRUE}, a list with \code{mu} and \code{sigma}.
#'
#' @examples
#' x <- matrix(rnorm(20), ncol = 2)
#' scaled_data <- scale(x)
#'
#' @export
#' @importFrom stats sd na.rm
#' @importFrom base apply

scale <- function(x, axis = 1, ddof = 1, method = "auto", mu = "default", sigma = "default", return_mu_sigma = FALSE) {
  x <- as.matrix(x)

  # Simplify if we transpose X if axis=1
  if (axis == 1) {
    x <- t(x)
  }

  # Expand dimension if array is 1d
  if (length(dim(x)) == 1) {
    x <- matrix(x, ncol = 1)
  }

  # Calculate mu and sigma if set to 'default' (ignoring nans)
  if (mu == "default") {
    mu <- colMeans(x, na.rm = TRUE)
  }
  if (sigma == "default") {
    sigma <- apply(x, 2, function(col) sd(col, na.rm = TRUE) * sqrt(length(col) / (length(col) - ddof)))
    sigma <- ifelse(sigma == 0, 1, sigma)  # if a value in sigma equals 0 it is converted to 1
  }

  # Error check before scaling
  if (length(mu) != ncol(x)) {
    stop("Length of mu array does not match x matrix.")
  }
  if (length(sigma) != ncol(x)) {
    stop("Length of sigma array does not match x matrix.")
  }

  # Scale based on selected method
  if (method == "auto") {
    z <- (x - mu) / sigma
  } else if (method == "pareto") {
    z <- (x - mu) / sqrt(sigma)
  } else if (method == "vast") {
    z <- ((x - mu) / sigma) * (mu / sigma)
  } else if (method == "level") {
    z <- (x - mu) / mu
  } else if (method == "range") {
    z <- (x - mu) / (apply(x, axis, max, na.rm = TRUE) - apply(x, axis, min, na.rm = TRUE))
  } else {
    stop("Method has to be either 'auto', 'pareto', 'vast', 'level' or 'range'.")
  }

  # Return x if axis = 1
  if (axis == 1) {
    z <- t(z)
  }

  if (return_mu_sigma) {
    return(list(mu = mu, sigma = sigma))
  } else {
    return(z)
  }
}

