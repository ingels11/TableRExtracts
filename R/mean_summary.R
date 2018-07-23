#' Create codebooks for datasets marked up by the TableRExtracts package
#'
#' A set of functions that will help to create codebooks...
#'
#' @param v Vector of values in a dataset
#' @param maxDecimals Maximum number of decimals to display (default = 2)
#' @export
meanSummary <- function(v, maxDecimals = 2) {
  UseMethod("meanSummary")
}

#' @export
meanSummary.numeric <- function(v, maxDecimals = 2) {
  meanSummaryHelper(v, maxDecimals)
}

# meanSummary <- summaryFunction(meanSummary,
#                                description = "Compute arithmetic mean")

# integer
meanSummary.integer <- function(v, maxDecimals = 2) {
  meanSummaryHelper(v, maxDecimals)
}

meanSummaryHelper <- function(v, maxDecimals) {
  # remove missing observations
  v <- na.omit(v)

  # compute mean and store "raw" output in `val`
  val <- mean(v)

  # store printable output in `res`
  res <- round(val, maxDecimals)

  # output summaryREsults
  dataMaid::summaryResult(list(feature = "Mean", result = res, value = val))
}




