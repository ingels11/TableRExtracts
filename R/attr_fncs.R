#' Add, remove, and get dataset, variable, and value value
#'
#' A set of functions that allow the user to document the variables and
#' associated value in a datset. The user has the option to add or remove
#' both variable and value value as well as return the current value.
#' Creating these value is useful in the other functions in the TableRExtracts
#' package.
#'
#' @param x a tibble
#' @param index either the name of a column/variable or index number
#' @param value variable value name as character vector
#' @param value variable value names as vector of character vectors
#' @param value value value as list
#' @param labels_lst value value as list of lists
#' @param val specific value of a variable for which a value is to be returned
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
#' the dataset as a tibble (x), the name of the variable as a character or a
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
#' college_grads <- AddValLabel(college_grads, "Major_category2") <-
#'       list(vals = c(1, 2, 3, 4, 5, 6),
#'            labs = c("Business, Education, and Communication",
#'                     "Education", "Health", "Natural Sciences",
#'                     "Social Sciences and Art", "Other"))
#' lst <- list("Major_category2" = list(vals = c(1, 2, 3, 4, 5, 6),
#'                                      labs = c("Business, Education, and Communication",
#'                                      "Education",
#'                                      "Health",
#'                                      "Natural Sciences",
#'                                      "Social Sciences and Art",
#'                                      "Other")),
#'             "Major_category" = list(vals = seq.int(from = 1, to = 16, by = 1),
#'                                     labs = "Agriculture & Natural Resources",
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
#'                                            "Social Science"))
#'
#' college_grads <- AddValLabels(college_grads) <- lst
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
GetDatLabel <- function(x) {

  if (is.null(attr(x, "label"))) {
    return("")
  } else {
    return(attr(x, "label"))
  }
}

#' @rdname GetDatLabel
#' @export
GetDatDescrip <- function(x) {

  if (is.null(attr(x, "shortDescription"))) {
    return("")
  } else {
    return(attr(x, "shortDescription"))
  }
}

#' @rdname GetDatLabel
#' @export
GetVarLabel <- function(x, index) {

  if (is.character(index) | is.numeric(index)) {
    if (length(index) == 1) {
      if (is.null(attr(x[[index]], "label"))) {
        return("")
      } else {
        return(attr(x[[index]], "label"))
      }
    } else if (length(index) > 1) {
      return(GetVarLabels(x[, index]))
    } else {
      return("")
    }
  } else {
    stop("In GetVarlabel, index argument must be a character vector")
  }
}

#' @rdname GetDatLabel
#' @export
GetVarLabels <- function(x) {

  labs <- c()
  for (i in 1:ncol(x)) {
    if (!is.null(GetVarLabel(x, i))) {
      labs <- c(labs, GetVarLabel(x, i))
    }
  }
  return(labs)
}

#' @rdname GetDatLabel
#' @export
GetVarDescrip <- function(x, index) {

  if (is.character(index) | is.numeric(index)) {
    if (is.null(attr(x[[index]], "shortDescription"))) {
      return("")
    } else {
      return(attr(x[[index]], "shortDescription"))
    }
  } else {
    stop("In GetVarDescrip, index argument must be a character vector")
  }

}

#' @rdname GetDatLabel
#' @export
GetValLabel <- function(x, index, val) {

  tmp <- attr(x[[index]], "vallabel")
  if (is.null(tmp$labs[tmp$vals == val])) {
    return("")
  } else {
    return(tmp$labs[tmp$vals == val])
  }
}

#' @rdname GetDatLabel
#' @export
GetValLabels <- function(x, index) {

  if (length(index) == 1) {
    if (is.null(attr(x[[index]], "vallabel"))) {
      return("")
    } else {
      return(attr(x[[index]], "vallabel"))
    }
  } else if (length(index) > 1) {
    return_lst <- list()
    for (i in index) {
      return_lst[[i]] <- attr(x[[i]], "vallabel")
    }
    return(return_lst)
  }
}

#' @rdname GetDatLabel
#' @export
GetLabel <- function(x, index, val = NA) {

  if (is.na(val)) {
    return(GetVarlabel(x, index))
  } else {
    return(GetVallabel(x, index, val))
  }
}

#' @rdname GetDatLabel
#' @export
`AddDatLabel<-` <- function(x, value) {

  if (!is.character(value)) {
    stop("In AddDatLabel, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  attr(x, "label") <- value
  x
}

#' @rdname GetDatLabel
#' @export
`AddDatDescrip<-` <- function(x, value) {

  if (!is.character(value)) {
    stop("In AddDatLabel, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  attr(x, "shortDescription") <- value
  x
}

#' @rdname GetDatLabel
#' @export
`AddVarLabel<-` <- function(x, index, value) {

  if (!is.character(value)) {
    stop("In AddVarlabel, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  if (is.character(index) | is.numeric(index)) {
    attr(x[[index]], "label") <- value
  } else {
    stop("In AddVarlabel, index argument must be a character vector")
  }
  x
}

#' @rdname GetDatLabel
#' @export
`AddVarLabels<-` <- function(x, stop.null = FALSE, value) {

  for (i in 1:length(value)) {
    if (stop.null & is.null(x[[names(value)[i]]])) {
      stop(paste(names(value)[i], " not a column/variable in object x"))
    } else {
      if (!is.null(x[[names(value)[i]]])) {
        AddVarLabel(x, names(value)[i]) <- value[i]
      }
    }
  }
  x
}

#' @rdname GetDatLabel
#' @export
`AddVarDescrip<-` <- function(x, index, value) {

  if (!is.character(value)) {
    stop("In AddVarDescrip, value argument must be a character vector")
  }
  value <- stringr::str_replace(value, ",", ";")
  if (is.character(index) | is.numeric(index)) {
    attr(x[[index]], "shortDescription") <- value
  } else {
    stop("In AddVarDescrip, index argument must be a character vector")
  }
  x
}

#' @rdname GetDatLabel
#' @export
`AddVarDescrips<-` <- function(x, value) {

  for (i in 1:length(value)) {
    if (is.null(x[[names(value)[i]]])) {
      stop(paste(names(value)[i], " not a column/variable in object x"))
    }
    AddVarDescrip(x, names(value)[i]) <- value[i]
  }
  x
}

#' @rdname GetDatLabel
#' @export
`AddValLabel<-` <- function(x, index, value) {

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
    attr(x[[index]], "vallabel") <- value
  } else {
    stop("In AddValLabel, index argument must be a character vector")
  }
  x
}

#' @rdname GetDatLabel
#' @export
`AddValLabels<-` <- function(x, value) {
  for (i in 1:length(value)) {
    AddValLabel(x, names(value)[i]) <- value[[i]]
  }
  x
}

#' @rdname GetDatLabel
#' @export
`RemoveDatLabel<-` <- function(x) {
### function does not work
  attr(x, "label") <- NULL
  x
}

#' @rdname GetDatLabel
#' @export
`RemoveDatDescrip<-` <- function(x) {
### function does not work
  attr(x, "shortDescription") <- NULL
  x
}

#' @rdname GetDatLabel
#' @export
`RemoveVarLabel<-` <- function(x, value) {

  if (is.character(value) | is.numeric(value)) {
    attr(x[[value]], "label") <- NULL
  } else {
    stop("In RemoveVarlabel, value argument must be a character vector")
  }
  x
}

#' @rdname GetDatLabel
#' @export
`RemoveVarDescrip<-` <- function(x, value) {

  if (is.character(value) | is.numeric(value)) {
    attr(x[[value]], "shortDescription") <- NULL
  } else {
    stop("In RemoveVarDescrip, value argument must be a character vector")
  }
  x
}

#' @rdname GetDatLabel
#' @export
`RemoveVallabel<-` <- function(x, value) {

  if (is.character(value) | is.numeric(value)) {
    attr(x[[value]], "vallabel") <- NULL
  } else {
    stop("In RemoveVallabel, value argument must be a character vector")
  }
  x
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
