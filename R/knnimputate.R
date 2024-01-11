#' kNN Missing Value Imputation
#'
#' Impute missing values in a matrix using kNN (k-Nearest Neighbors) imputation based on Euclidean distance.
#'
#' @param x An array-like object that contains the data with NaNs.
#' @param k A positive integer excluding 0 (default: 3). The number of nearest neighbors to use.
#'
#' @return An array-like object corresponding to x with NaNs imputed.
#'
#' @examples
#' x <- matrix(c(1, 2, 3, NA, 5, 6, 7, 8, 9), ncol = 3)
#' imputed_data <- knnimpute(x, k = 2)
#'
#' @export
#'
#' @importFrom stats dist
#' @importFrom base order
#' @importFrom base weighted.mean
#' @importFrom base is.na
#' @importFrom base warning

knnimpute <- function(x, k = 3) {
  # Tranpose x so we treat columns as features, and rows as samples
  x <- t(x)

  k <- as.integer(k)
  # Error check for k value
  if (!is.integer(k)) {
    stop("k is not an integer")
  }
  if (k < 1) {
    stop("k must be greater than zero")
  }
  k_max <- ncol(x) - 1
  if (k_max < k) {
    stop(sprintf("k value is too high. Max k value is %d", k_max))
  }

  # z is the returned array with NaNs imputed
  z <- x

  # Use columns without NaNs for knnimpute
  nan_check <- is.nan(x)
  no_nan <- apply(!nan_check, 2, all)

  # Error check that not all columns have NaNs
  x_no_nan <- x[, no_nan]
  if (length(x_no_nan) == 0) {
    stop("All colummns of the input data contain missing values. Unable to impute missing values.")
  }

  # Calculate pairwise distances between columns, and covert to square-form distance matrix
  pair_dist <- dist(t(x_no_nan), method = "euclidean")
  sq_dist <- as.matrix(pair_dist)

  # Make diagonals negative and sort
  dist <- t(apply(sq_dist - diag(ncol(sq_dist)), 2, function(x) sort(x)))
  dist_idx <- t(apply(sq_dist - diag(ncol(sq_dist)), 2, function(x) order(x)))

  # Find where neighbours are equal distance
  equal_dist_a <- apply(diff(dist[2:nrow(dist), ], 1), 2, function(x) x == 0)
  equal_dist_a <- as.integer(equal_dist_a)
  equal_dist_b <- rep(0, nrow(dist))
  equal_dist <- cbind(equal_dist_a, equal_dist_b)

  # Get rows and cols for missing values
  nan_idx <- which(nan_check, arr.ind = TRUE)
  nan_rows <- nan_idx[, 1]
  nan_cols <- nan_idx[, 2]

  # Make sure rows/cols are in a list (note: this happens when there is 1 missing value)
  if (!is.vector(nan_rows)) {
    nan_rows <- as.vector(nan_rows)
    nan_cols <- as.vector(nan_cols)
  }

  # Impute each NaN value
  for (i in seq_along(nan_rows)) {
    # Error check for rows with all NaNs
    if (all(is.nan(x[nan_rows[i], ]))) {
      warning(sprintf("Row %d contains all NaNs, so Row %d is imputed with zeros.", nan_rows[i], nan_rows[i]))
    }

    # Create a loop from 1 to nrow(dist_idx) - k
    lastk <- nrow(dist_idx) - k
    loopk <- seq(1, lastk)

    # Impute
    for (j in loopk) {
      L_a <- equal_dist[j + k - 2, nan_cols[i]:ncol(dist_idx)]
      L <- which(L_a == 0)[1]  # equal_dist neighbours

      x_vals_r <- nan_rows[i]
      x_vals_c <- dist_idx[j:(j + k + L), nan_cols[i]]
      x_vals <- x[x_vals_r, x_vals_c]
      weights <- 1 / dist[1:(k + L), nan_cols[i]]
      imp_val <- weighted.mean(x_vals, weights, na.rm = TRUE)  # imputed value

      if (!is.na(imp_val)) {
        z[nan_rows[i], nan_cols[i]] <- imp_val
        break
      }
    }
  }

  # Transpose z
  z <- t(z)
  return(z)
}
