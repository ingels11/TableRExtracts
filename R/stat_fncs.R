#' Return the First Quartile
#'
#' @param x Array of numeric data.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return A number representing the first quartile of the data in x.
#' @examples
#' q1(nycflights13::flights$distance)
#' @export
q1 <- function(x, na.rm = TRUE) {

  if (sum(is.na(x)) > 0 & !na.rm) {
    return(NA)
  } else {
    return(stats::quantile(x, na.rm = na.rm)[[2]])
  }
}

#' Return the Third Quartile
#'
#' @param x Array of numeric data.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return A number representing the third quartile of the data in x.
#' @examples
#' q3(nycflights13::flights$distance)
#' @export
q3 <- function(x, na.rm = TRUE) {

  if (sum(is.na(x)) > 0 & !na.rm) {
    return(NA)
  } else {
    return(stats::quantile(x, na.rm = na.rm)[[4]])
  }
}

#' Length of an Object (Excluding Missing Values)
#'
#' @param x Array of numeric data.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return A number representing the number of non-missing values in an array.
#' @examples
#' length_nona(nycflights13::flights$air_time)
#'
#' ### Compare this result to the following
#' length(nycflights13::flights$air_time)
#' @export
length_nona <- function(x, na.rm = TRUE) {
  return(sum(!is.na(x)))
}

#' Apply a Function to an Array, For Extract Creation
#'
#' @param df a tibble
#' @param index variable name in x to apply the specified function
#' @param margin variable name in x to stratify data in index
#' @param tab.max for a given extract, largest number of unique stratiying values
#' @param fnc the function to be applied
#' @param ... optional arguments to fun
#' @return An array including results of function specified as fun, on variable
#'   specified as index, stratified by margin, first element is for entire
#'   array, remaining elements are for stratified
#' @examples
#' create_data_tab(nycflights13::flights, "air_time", "origin", 3, mean)
#' @export
create_data_tab <- function(df, index, margin, tab.max, fnc, ...) {

  # df must be a tibble
  if (!tibble::is.tibble(df)) {
    if (is.data.frame(df)) {
      df <- tibble::as.tibble(df)
    } else {
      stop("In create_data_tab, argument df must be a data frame or tibble")
    }
  }
  if (!is.function(fnc)) {
    stop("In create_data_tab, argument fnc must be a function")
  }

  index_vec <- dplyr::pull(df, index)

  if (is.na(margin)) {

    data.tab <- fnc(index_vec, ...)
    names(data.tab) <- "Overall"
    return(data.tab)

  } else {

    margin_vec <- dplyr::pull(df, margin)
    data.tab <- tapply(index_vec, margin_vec, fnc, ...)

    data.tab <- c(data.tab, rep(NA, tab.max - length(data.tab)))
    data.tab <- c(data.tab, fnc(index_vec, ...))
    names(data.tab)[length(data.tab)] <- "Overall"

    return(data.tab)
  }

}

getsig <- function(pval, symbol = "*", cuts = c(0.001, 0.01, 0.05)) {

  cuts <- sort(cuts)
  cuts_len <- length(cuts)
  for (i in 1:cuts_len) {
    if (pval <= cuts[i]) return(stringr::str_c(rep(symbol, cuts_len + 1 - i),
                                               collapse = ""))
  }
  return("")
}

create_cat_var <- function(df, x, type = "binary:median", right = TRUE) {

  if (length(stringr::str_split(type, ":")[[1]]) == 2) {
    type1 <- stringr::str_split(type, ":")[[1]][1]
    type2 <- stringr::str_split(type, ":")[[1]][2]
  } else if (length(stringr::str_split(type, ":")[[1]]) == 1) {
    type1 <- stringr::str_split(type, ":")[[1]][1]
  } else {
    stop("Invalid type argument")
  }

  if (type1 == "binary") {
    if (type2 == "mean") {
      if (right) {
        df[[paste(x, type1, type2, sep = "_")]] <-
          ifelse(df[[x]] <= mean(df[[x]], na.rm = TRUE), 1,
                 ifelse(is.na(df[[x]]), NA, 0))
      } else {
        df[[paste(x, type1, type2, sep = "_")]] <-
          ifelse(df[[x]] < mean(df[[x]], na.rm = TRUE), 1,
                 ifelse(is.na(df[[x]]), NA, 0))
      }

    } else {
      df[[paste(x, type1, type2, sep = "_")]] <-
        ifelse(df[[x]] < median(df[[x]], na.rm = TRUE), 1,
               ifelse(is.na(df[[x]]), NA, 0))
    }
  }
  return(df)
}
