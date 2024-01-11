#' Introduction to Data Preprocessing and PCA Analysis
#'
#' This vignette provides an example of common data preprocessing steps and demonstrates the use of PCA analysis using the `yourPackage` package.
#'
#' @seealso [MeTaStaT](https://github.com/danymukesha/metastat)
#'
#' @examples
#' # Load required libraries
#' library(metastat)
#'
#' # Load or generate your data
#' data <- YOUR_DATA_LOADING_FUNCTION()
#'
#' # Log scale transformation (base-10)
#' Xlog <- log10(data)
#'
#' # Scale the data using different methods (auto, pareto, vast, level)
#' Xscale <- metastat::scale(Xlog, method = 'auto')
#'
#' # Perform kNN imputation (k=3) for missing values
#' Xknn <- metastat::knnimpute(Xscale, k = 3)
#'
#' # Print the dimensions of the imputed data
#' cat("Xknn:", nrow(Xknn), "rows &", ncol(Xknn), "columns\n")
#'
#' # Perform PCA analysis
#' metastat::pca_plot(Xknn, pcx = 1, pcy = 2, group_label = dataTable$SampleType)
#' @export

