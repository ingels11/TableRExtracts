#' Create a properly formatted date and time string
#'
#' @return The current date and time
#' @examples
#' GetDateTime()
GetDateTime <- function() {
  Sys.setenv(TZ = "America/New_York")
  return(c(paste(Sys.Date()),
           format(Sys.time(), "%X")))
}

#' Maximum # of Unique Values in a List of Stratifying Variables
#'
#' @param x a dataframe
#' @param strat_var character vector of a list of variables in x
#' @return The number of unique values in variable in strat_var list of variables
#'   with largest number of unique values
#'
get_max_unique <- function(x, strat_var) {

  rtn <- c()
  for (i in strat_var) {
    rtn <- c(rtn, length(unique(x[, i])[[1]]))
  }
  return(max(rtn, na.rm = TRUE))
}

#' Check for Subdirectory "Extracts" and Create If Necessary
#'
#' @return nothing
check_extracts_dir <- function() {
  if (!file.exists("Extracts")) {
    dir.create("Extracts")
  }
}

getFunctionName <- function(fnc) {

  tmp <- deparse(fnc)
  if (length(tmp) == 1) {
    #return(tmp[[1]])
    return(stringr::str_extract(stringr::str_extract(tmp,
                                                     "\"[a-zA-Z]+\""),
                                "[a-zA-Z]+"))
  } else {
    return(stringr::str_extract(stringr::str_extract(tmp[[2]],
                                                     "\"[a-zA-Z]+\""),
                                "[a-zA-Z]+"))
  }

}
