#' Functions used to add percentage of zeroes in variable to codebooks
#'
#' A set of functions that will help to add the line of percentage of zero
#' values for a variable to codebooks.
#'
#' @param v Vector of values in a dataset
#' @param maxDecimals Maximum number of decimals to display (default = 2)
#' @export
pctZeroes <- function(v, maxDecimals = 2) {
  UseMethod("pctZeroes")
}

# numeric
pctZeroes.numeric <- function(v, maxDecimals = 2) {
  pctZeroesHelper(v, maxDecimals)
}

# integer
pctZeroes.integer <- function(v, maxDecimals = 2) {
  pctZeroesHelper(v, maxDecimals)
}

pctZeroesHelper <- function(v, maxDecimals) {
  # remove missing observations
  v <- stats::na.omit(v)

  # compute mean and store "raw" output in `val`
  val <- sum(v == 0) / length(v)

  # store printable output in `res`
  res <- round(val, maxDecimals)

  # output summaryREsults
  dataMaid::summaryResult(list(feature = "PctZero", result = res, value = val))
}




