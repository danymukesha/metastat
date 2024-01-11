#' Univariate 2-Class Analysis
#'
#' Creates a table of univariate statistics for a two-class analysis.
#'
#' @name univariate_2class
#'
#' @param DataTable Data frame containing the required columns.
#' @param PeakTable Data frame containing peak information.
#' @param group Name of the column in DataTable that contains the class data.
#' @param posclass Name of the positive class in the group column.
#' @param parametric Logical, if TRUE, parametric statistics are calculated; if FALSE, non-parametric statistics are calculated.
#' @param seed Integer or NULL, used to seed the generator for the bootstrap (resample with replacement) when calculating non-parametric statistics.
#'
#' @return A data frame with univariate statistics for each peak.
#'
#' @examples
#' \dontrun{
#' # Example usage
#' result <- univariate_2class(DataTable, PeakTable, group = "Class", posclass = "Positive")
#' }
#'
#' @seealso \code{\link{table_check}}, \code{\link{mean_ci}}, \code{\link{quantile_ci}}
#' @importFrom dplyr %>%
#' @importFrom purrr map_dbl
#' @importFrom stats wilcox.test shapiro.test
#' @importFrom broom tidy
#'
#' @export

library(dplyr)
library(tidyr)
library(purrr)
library(broom)

univariate_2class <- function(DataTable, PeakTable, group, posclass, parametric = TRUE, seed = NULL) {

  # Error checks
  if (!all(c("Idx", "Name", "Label") %in% colnames(PeakTable))) {
    stop("PeakTable should have columns 'Idx', 'Name', and 'Label'")
  }
  if (!group %in% colnames(DataTable)) {
    stop(paste("Column '", group, "' does not exist in DataTable", sep = ""))
  }
  if (!(posclass %in% unique(DataTable[[group]]))) {
    stop(paste("Positive class '", posclass, "' not found in '", group, "' column.", sep = ""))
  }
  if (length(unique(DataTable[[group]])) != 2) {
    stop(paste("Column '", group, "' should have exactly 2 groups", sep = ""))
  }

  # Get x0, x1
  peaklist <- PeakTable$Name
  x <- DataTable[, peaklist, drop = FALSE]
  y <- DataTable[[group]]
  x1 <- x[y == posclass, ]
  x0 <- x[y != posclass, ]

  # Create stats table (include Idx, Name, and Label)
  StatsTable <- PeakTable %>%
    select(Idx, Name, Label) %>%
    as_tibble()

  if (parametric) {
    # Calculate mean and std
    StatsTable <- StatsTable %>%
      mutate(
        Grp0_Mean = colMeans(x0, na.rm = TRUE),
        Grp0_Mean_95CI = map_dbl(x0, ~ mean_ci(.x)),
        Grp1_Mean = colMeans(x1, na.rm = TRUE),
        Grp1_Mean_95CI = map_dbl(x1, ~ mean_ci(.x)),
        Sign = ifelse(median(x1, na.rm = TRUE) / median(x0, na.rm = TRUE) > 1, 1, 0),
        TTest = map2_dbl(x0, x1, ~ t.test(.x, .y, na.rm = TRUE)$statistic),
        TTestPvalue = map2_dbl(x0, x1, ~ t.test(.x, .y, na.rm = TRUE)$p.value),
        bhQvalue = p.adjust(StatsTable$TTestPvalue, method = "BH")
      )
  } else {
    # Calculate median
    StatsTable <- StatsTable %>%
      mutate(
        Grp0_Median = sapply(x0, median, na.rm = TRUE),
        Grp0_Median_95CI = map_dbl(x0, ~ quantile_ci(.x)),
        Grp1_Median = sapply(x1, median, na.rm = TRUE),
        Grp1_Median_95CI = map_dbl(x1, ~ quantile_ci(.x)),
        MedianFC = median(x1, na.rm = TRUE) / median(x0, na.rm = TRUE),
        Sign = ifelse(median(x1, na.rm = TRUE) / median(x0, na.rm = TRUE) > 1, 1, 0),
        MannWhitneyU = map2_dbl(x0, x1, ~ wilcox.test(.x, .y, na.rm = TRUE)$statistic),
        MannWhitneyPvalue = map2_dbl(x0, x1, ~ wilcox.test(.x, .y, na.rm = TRUE)$p.value),
        bhQvalue = p.adjust(StatsTable$MannWhitneyPvalue, method = "BH")
      )
  }

  # Calculate total missing and total missing %
  nannum <- colSums(is.na(x))
  nanperc <- nannum / nrow(x)
  StatsTable <- StatsTable %>%
    mutate(
      TotalMissing = nannum,
      PercTotalMissing = round(nanperc * 100, 3)
    )

  # Calculating missing % for group 0, and group 1...
  nanperc_0 <- colSums(is.na(x0)) / nrow(x0)
  nanperc_1 <- colSums(is.na(x1)) / nrow(x1)
  StatsTable <- StatsTable %>%
    mutate(
      Grp0_Missing = round(nanperc_0 * 100, 3),
      Grp1_Missing = round(nanperc_1 * 100, 3)
    )

  # Shapiro-Wilk
  shapiro_test <- function(x) {
    if (length(na.omit(x)) >= 3) {
      shapiro.test(x)$statistic
    } else {
      NA
    }
  }

  StatsTable <- StatsTable %>%
    mutate(
      ShapiroW = map_dbl(x, shapiro_test),
      ShapiroPvalue = map_dbl(x, ~ shapiro.test(.x)$p.value)
    )

  # Levenes
  levene_test <- function(x0, x1) {
    if (length(na.omit(x0)) >= 3 && length(na.omit(x1)) >= 3) {
      stats::levene(x0, x1)$statistic
    } else {
      NA
    }
  }

  StatsTable <- StatsTable %>%
    mutate(
      LeveneW = map2_dbl(x0, x1, levene_test),
      LevenePvalue = map2_dbl(x0, x1, ~ stats::levene(.x, .y)$p.value)
    )

  return(StatsTable)
}

# # Helper functions
# mean_ci <- function(x) {
#   mean_x <- mean(x, na.rm = TRUE)
#   std_err <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
#   ci_width <- 1.96 * std_err
#   c(mean_x - ci_width, mean_x + ci_width)
# }
#
# quantile_ci <- function(x) {
#   q25 <- quantile(x, 0.25, na.rm = TRUE)
#   q75 <- quantile(x, 0.75,
# }
