#' Add, remove, and get dataset, variable, and value value
#'
#' A set of functions that allow the user to document the variables and
#' associated value in a datset. The user has the option to add or remove
#' both variable and value value as well as return the current value.
#' Creating these value is useful in the other functions in the TableRExtracts
#' package.
#'
#' @param df a tibble
#' @param index either the name of a column/variable or index number
#' @param value variable value name as character vector
#' @param val specific value of a variable for which a value is to be returned
#' @param stop.null optional argument that when true will stop function
#'    execution if a variable listed in value does not exist in df
#' @details
#' Labels for elements of a datset are set using attributes
#' (see \code{\link[base]{attributes}}). There are three main types of labels
#' that can be set (attribute name in parentheses):
#' \itemize{
#'   \item Variable labels are short, designed for tables (label)
#'   \item Variable text is typically longer and more descirptive, designed
#'   for codebooks (shortDescription)
#'   \item Value labels are short, including one for each unique value a
#'   variable may take, designed for tables (vallabel)
#' }
#'
#' Several prefixes, bodies, and suffixes are used throughout with function
#' names to allow for straightforward understanding of the utility of each
#' function. Functions then are of the form PrefixBodySuffix.
#'
#' Prefixes include the following:
#' \itemize{
#'   \item \emph{Add}: Attach one or more value to variables or value
#'   \item \emph{Remove}: Remove or detach a value from a variable or value
#'   \item \emph{Get}: Retrieve and return a value for a speciified variable
#'   or value
#' }
#'
#' Bodies include the following:
#' \itemize{
#'   \item \emph{Dat}: Dataset descriptors
#'   \item \emph{Var}: Variable descriptors
#'   \item \emph{Val}: Variable value descriptors (categorical variables)
#' }
#'
#' Suffixes include the following:
#' \itemize{
#'   \item \emph{Label}: Create short labels used primarily in tables but
#'   also in codebooks
#'   \item \emph{Descrip}: Create longer labels used primarily in codebooks
#' }
#' Further, plural suffixes allow for setting value or texts for more than
#' one variable in a single call by iteratively calling the non-plural version
#' of the function.
#'
#' \strong{Variable labels:}
#'
#' Variable labels are set using AddVarLabel() and the arguments must include
#' the dataset as a tibble (df), the name of the variable as a character or a
#' number that refers to its position in the dataset (index), and the value as
#' a character vector (value). This label will be passed to extracts when
#' the variable is included and is ideal for referencing the variable in a table.
#'
#' Variable texts are set using AddVarDescrip() and includes the same arguments as
#' with AddVarlabel(). This text will be inlcuded in a codebook created for the
#' dataset when the associated variable is inlcuded.
#'
#' \strong{value value:}
#'
#' value value are set
#'
#' @return
#' No return for functions with Add or Remove
#' Functions with Get return a character vector.
#'
#' @examples
#' ### Add datasert label
#' AddDatLabel(college_grads) <- "College Graduate Earnings"
#' AddDatDescrip(college_grads) <-
#'    "Basic earnings and labor force information for recent college graduates"
#'
#' ### Add variable label either by variable name or location in the dataset
#' AddVarLabel(college_grads, "Total") <- "Total number of people with major"
#' AddVarLabel(college_grads, 3) <- "Total number of people with major"
#' AddVarLabels(college_grads) <- c("Total" = "Total number of people with major",
#'                                  "Men" = "Male graduates",
#'                                  "Women" = "Female graduates")
#'
#' ### Add value labels
#' AddValLabel(college_grads, "Major_category2") <-
#'        list(vals = c(1, 2, 3, 4, 5, 6),
#'             labs = c("Business, Education, and Communication",
#'                      "Education", "Health", "Natural Sciences",
#'                      "Social Sciences and Art", "Other"))
#' lst <- list("Major_category2" = list(vals = c(1, 2, 3, 4, 5, 6),
#'                                      labs = c("Business, Education, and Communication",
#'                                      "Education",
#'                                      "Health",
#'                                      "Natural Sciences",
#'                                      "Social Sciences and Art",
#'                                      "Other")),
#'             "Major_category" = list(vals = seq.int(from = 1, to = 16, by = 1),
#'                                     labs = c("Agriculture & Natural Resources",
#'                                            "Arts",
#'                                            "Biology & life Science",
#'                                            "Business",
#'                                            "Communications & Journalism",
#'                                            "Computers & Mathematics",
#'                                            "Education",
#'                                            "Engineering",
#'                                            "Health",
#'                                            "Humanities & Liberal Arts",
#'                                            "Industrial Arts & Consumer Services",
#'                                            "Interdisciplinary",
#'                                            "Law & Public Policy",
#'                                            "Physical Sciences",
#'                                            "Psychology & Social Work",
#'                                            "Social Science")))
#'
#' AddValLabels(college_grads) <- lst
#'
#' ### Return variable and value value
#' ### Two ways to return, either by specifying Var or Val directly
#' ### or indirectly through the Getlabel() function
#' GetVarLabel(college_grads, "Total")
#' GetValLabel(college_grads, "Major_category2", 1)
#' GetLabel(college_grads, "Total")
#' GetLabel(college_grads, "Major_category2", 1)
#'
#' ### Remove variable and value value
#' RemoveVarLabel(college_grads) <- "Total"
#' RemoveValLabel(college_grads) <- "Major_category2"
#' @export
GetDatLabel <- function(df) {

  if (is.null(attr(df, "label"))) {
    return("")
  } else {
    return(attr(df, "label"))
  }
}

#' @rdname GetDatLabel
#' @export
GetDatDescrip <- function(df) {

  if (is.null(attr(df, "shortDescription"))) {
    return("")
  } else {
    return(attr(df, "shortDescription"))
  }
}

#' @rdname GetDatLabel
#' @export
GetVarLabel <- function(df, index) {

  if (is.character(index) | is.numeric(index)) {
    if (length(index) == 1) {
      if (is.null(attr(df[[index]], "label"))) {
        return("")
      } else {
        return(attr(df[[index]], "label"))
      }
    } else if (length(index) > 1) {
      return(GetVarLabels(df[, index]))
    } else {
      return("")
    }
  } else {
    stop("In GetVarlabel, index argument must be a character vector")
  }
}

#' @rdname GetDatLabel
#' @export
GetVarLabels <- function(df) {

  labs <- c()
  for (i in 1:ncol(df)) {
    if (!is.null(GetVarLabel(df, i))) {
      labs <- c(labs, GetVarLabel(df, i))
    }
  }
  return(labs)
}

#' @rdname GetDatLabel
#' @export
GetVarDescrip <- function(df, index) {

  if (is.character(index) | is.numeric(index)) {
    if (is.null(attr(df[[index]], "shortDescription"))) {
      return("")
    } else {
      return(attr(df[[index]], "shortDescription"))
    }
  } else {
    stop("In GetVarDescrip, index argument must be a character vector")
  }

}

#' @rdname GetDatLabel
#' @export
GetValLabel <- function(df, index, val) {

  tmp <- attr(df[[index]], "vallabel")
  if (is.null(tmp$labs[tmp$vals == val])) {
    return("")
  } else {
    return(tmp$labs[tmp$vals == val])
  }
}

#' @rdname GetDatLabel
#' @export
GetValLabels <- function(df, index) {

  if (length(index) == 1) {
    if (is.null(attr(df[[index]], "vallabel"))) {
      return("")
    } else {
      return(attr(df[[index]], "vallabel"))
    }
  } else if (length(index) > 1) {
    return_lst <- list()
    for (i in index) {
      return_lst[[i]] <- attr(df[[i]], "vallabel")
    }
    return(return_lst)
  }
}

#' @rdname GetDatLabel
#' @export
GetLabel <- function(df, index, val = NA) {

  if (is.na(val)) {
    return(GetVarLabel(df, index))
  } else {
    return(GetValLabel(df, index, val))
  }
}

#' @rdname GetDatLabel
#' @export
`AddDatLabel<-` <- function(df, value) {

  if (!is.character(value)) {
    stop("In AddDatLabel, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  attr(df, "label") <- value
  df
}

#' @rdname GetDatLabel
#' @export
`AddDatDescrip<-` <- function(df, value) {

  if (!is.character(value)) {
    stop("In AddDatLabel, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  attr(df, "shortDescription") <- value
  df
}

#' @rdname GetDatLabel
#' @export
`AddVarLabel<-` <- function(df, index, value) {

  if (!is.character(value)) {
    stop("In AddVarlabel, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  if (is.character(index) | is.numeric(index)) {
    attr(df[[index]], "label") <- value
  } else {
    stop("In AddVarlabel, index argument must be a character vector")
  }
  df
}

#' @rdname GetDatLabel
#' @export
`AddVarLabels<-` <- function(df, stop.null = FALSE, value) {

  for (i in 1:length(value)) {
    if (stop.null & is.null(df[[names(value)[i]]])) {
      stop(paste(names(value)[i], " not a column/variable in object df"))
    } else {
      if (!is.null(df[[names(value)[i]]])) {
        AddVarLabel(df, names(value)[i]) <- value[i]
      }
    }
  }
  df
}

#' @rdname GetDatLabel
#' @export
`AddVarDescrip<-` <- function(df, index, value) {

  if (!is.character(value)) {
    stop("In AddVarDescrip, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  if (is.character(index) | is.numeric(index)) {
    attr(df[[index]], "shortDescription") <- value
  } else {
    stop("In AddVarDescrip, index argument must be a character vector")
  }
  df
}

#' @rdname GetDatLabel
#' @export
`AddVarDescrips<-` <- function(df, value) {

  for (i in 1:length(value)) {
    if (is.null(df[[names(value)[i]]])) {
      stop(paste(names(value)[i], " not a column/variable in object df"))
    }
    AddVarDescrip(df, names(value)[i]) <- value[i]
  }
  df
}

#' @rdname GetDatLabel
#' @export
`AddValLabel<-` <- function(df, index, value) {

  if (!is.list(value)) {
    stop("In AddValLabel, value argument must be a list")
  }
  if (length(value$vals) != length(value$labs)) {
    stop("In AddValLabel, vals and labs must be of the same length")
  }
  for (i in 1:length(value$labs)) {
    value$labs[i] <- stringr::str_replace_all(value$labs[i], ",", ";")
  }
  if (is.character(index) | is.integer(index)) {
    attr(df[[index]], "vallabel") <- value
  } else {
    stop("In AddValLabel, index argument must be a character vector")
  }
  df
}

#' @rdname GetDatLabel
#' @export
`AddValLabels<-` <- function(df, value) {
  for (i in 1:length(value)) {
    AddValLabel(df, names(value)[i]) <- value[[i]]
  }
  df
}

#' @rdname GetDatLabel
#' @export
RemoveDatLabel <- function(df) {
### function does not work
  attr(df, "label") <- NULL
  return(df)
}

#' @rdname GetDatLabel
#' @export
RemoveDatDescrip <- function(df) {
### function does not work
  attr(df, "shortDescription") <- NULL
  return(df)
}

#' @rdname GetDatLabel
#' @export
`RemoveVarLabel<-` <- function(df, value) {

  if (is.character(value) | is.numeric(value)) {
    attr(df[[value]], "label") <- NULL
  } else {
    stop("In RemoveVarlabel, value argument must be a character vector")
  }
  df
}

#' @rdname GetDatLabel
#' @export
`RemoveVarDescrip<-` <- function(df, value) {

  if (is.character(value) | is.numeric(value)) {
    attr(df[[value]], "shortDescription") <- NULL
  } else {
    stop("In RemoveVarDescrip, value argument must be a character vector")
  }
  df
}

#' @rdname GetDatLabel
#' @export
`RemoveValLabel<-` <- function(df, value) {

  if (is.character(value) | is.numeric(value)) {
    attr(df[[value]], "vallabel") <- NULL
  } else {
    stop("In RemoveVallabel, value argument must be a character vector")
  }
  df
}

CombineAllLabels <- function(df) {

  # first for the dataset
  if (!is.null(GetDatDescrip(df))) {
    AddDatLabel(df) <- paste(GetDatLabel(df), GetDatDescrip(df), sep = ": ")
  }

  for (i in names(df)) {
    if (!is.null(GetVarDescrip(df, i))) {
      AddVarLabel(df, i) <-
        paste(GetVarLabel(df, i), GetVarDescrip(df, i), sep = ": ")
    }
  }
  return(df)
}
