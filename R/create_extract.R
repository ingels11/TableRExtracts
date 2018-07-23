#' Create a Table Extract
#'
#' A set of functions to create different types of table extracts. These extracts
#' are written to a comma-separated value (.csv) file.
#'
#' @param x a tibble
#' @param vars a character vector include one or more variable name in x
#' @param tab_vars a character vector including one or more variable name
#' @param notes a character string including notes to include in the extract
#' @param outputfile a character string for which to write the extract
#' @param info hmm
#' @param test (optional) the name of a statistical test to be conducted hwen
#' run_test is set to TRUE, if left as NA the default test will be used
#' @param run_test (optional) should an appropriate statistical test be conducted
#'
#' @details
#' There are several different types of table extracts that can be created
#'
#' @export
CreateExtractFreq <- function(x, vars, tab_vars, notes, outputfile, info,
                                test = NA, run_test = TRUE) {

  # Setup extract --------------------------------------------------------------
  # If an Extracts folder does not exist, create it
  # fileConn is the connection to the CSV file for the extract
  # If no output file is included, extract will be written to terminal
  if (!is.na(outputfile)) {
    check_extracts_dir()

    fileConn <- file(paste("Extracts/", outputfile, ".csv", sep = ""),
                     open = "wt")
  } else {
    fileConn <- NA
  }

  # Check the text provided in info, replace commas with semicolons
  if (!purrr::is_empty(info)) {
    names_info <- stringr::str_replace_all(names(info), ",", ";")
    info <- stringr::str_replace_all(info, ",", ";")
    names(info) <- names_info
    rm(names_info)
  }

  # Create a list with information used when writing extract lines
  extract_lst <- list(type = "frequency",
                      tab_vars = tab_vars,
                      tab_labels = GetVarLabel(x, tab_vars),
                      tab_val_labels = GetValLabels(x, tab_vars),
                      function_name = "CreateExtractFreq",
                      info = info,
                      dataset = as.character(substitute(x)),
                      dataset_label = GetDatLabel(x),
                      notes = notes)

  # tab_vars is a variable where each variable in vars is stratified
  # statistical tests are run for each var, stratified by each tab_var
  # if (!purrr::is_empty(tab_vars) & (sum(!is.na(tab_vars)) > 0)) {
  #   extract_lst$tab_vals <- unique(dplyr::pull(x, tab_vars))
  #   extract_lst$tab_vals <- extract_lst$tab_vals[order(extract_lst$tab_vals)]
  #   #tab_vals <- tab_vals[order(tab_vals)]
  # } else {
  #   extract_lst$tab_vals <- NA
  # }
  # tab.max is important as the extract includes columns for stratification
  # and the maximum number of unique values needs to be known

  extract_lst$tab.max = ifelse(sum(!is.na(tab_vars)) == 0,
                               0, get_max_unique(x, tab_vars))
  extract_lst$run_test = ifelse(extract_lst$tab.max == 0, FALSE, run_test)

  # Write extract lines --------------------------------------------------------

  # Write header line of the extract
  write_header_line(extract_lst, fileConn)

  # Loop over variables included in vars
  for (var in vars) {

    var_lst <- list(nobs = length(x[[var]]),
                    var_name = var,
                    var_label = GetVarLabel(x, var),
                    var_num = length(unique(x[[var]])))

    for (tab_var in tab_vars) {
      # List includes information specific to the current variable from vars
      var_lst$var_freq <- create_data_tab(x, var, tab_var, extract_lst$tab.max,
                                          length_nona)

      var_lst$tab_var <- tab_var
      var_lst$tab_label <- GetVarLabel(x, tab_var)
      var_lst$tab_val_label <- GetValLabels(x, tab_var)
      var_lst$tab_var_num <- length(unique(x[[tab_var]]))

      if (run_test & (var_lst$var_num > 1) & (var_lst$tab_var_num > 1)) {
        result <- chisq.test(table(dplyr::pull(x, var),
                                   dplyr::pull(x, tab_var)))
        result$statsig <- getsig(result$p.value)
      } else {
        result <- NA
      }

      # Loop over unique values in the current variable from vars
      for (val in sort(unique(x[[var]]))) {

        val_lst <- var_lst
        val_lst$val <- val
        val_lst$val_label <- GetValLabel(x, var, val)
        val_lst$val_freq <- create_data_tab(x[x[[var]] == val, ],
                                            var, tab_var, extract_lst$tab.max,
                                            length_nona)
        val_lst$result <- result

        write_line(extract_lst, val_lst, fileConn)
      }
    }
  }

  # Finish extract -------------------------------------------------------------
  if (!is.na(outputfile)) {
    close(fileConn)
  }

}

#' @export
CreateExtractMean <- function(x, vars, tab_vars, notes, outputfile, info,
                              stats = c(mean, median, min, max),
                              test = NA, run_test = TRUE) {

  # Setup extract --------------------------------------------------------------
  # If an Extracts folder does not exist, create it
  # fileConn is the connection to the CSV file for the extract
  # If no output file is included, extract will be written to terminal
  if (!is.na(outputfile)) {
    check_extracts_dir()

    fileConn <- file(paste("Extracts/", outputfile, ".csv", sep = ""),
                     open = "wt")
  } else {
    fileConn <- NA
  }

  # Check the text provided in info, replace commas with semicolons
  if (!purrr::is_empty(info)) {
    names_info <- stringr::str_replace_all(names(info), ",", ";")
    info <- stringr::str_replace_all(info, ",", ";")
    names(info) <- names_info
    rm(names_info)
  }

  # Create a list with information used when writing extract lines
  extract_lst <- list(type = "mean",
                      tab_vars = tab_vars,
                      tab_labels = GetVarLabel(x, tab_vars),
                      tab_val_labels = GetValLabels(x, tab_vars),
                      function_name = "CreateExtractMean",
                      info = info,
                      dataset = as.character(substitute(x)),
                      dataset_label = GetDatLabel(x),
                      notes = notes,
                      stats = stats)

  # tab.max is important as the extract includes columns for stratification
  # and the maximum number of unique values needs to be known
  extract_lst$tab.max = ifelse(sum(!is.na(tab_vars)) == 0,
                               0, get_max_unique(x, tab_vars))
  extract_lst$run_test = ifelse(extract_lst$tab.max == 0, FALSE, run_test)

  # Write extract lines --------------------------------------------------------

  # Write header line of the extract
  write_header_line(extract_lst, fileConn)

  # Loop over variables included in vars
  for (var in vars) {

    var_lst <- list(nobs = length(x[[var]]),
                    var_name = var,
                    var_label = GetVarLabel(x, var))

    for (tab_var in tab_vars) {
      # List includes information specific to the current variable from vars
      var_lst$var_freq <- create_data_tab(x, var, tab_var, extract_lst$tab.max,
                                          length_nona)

      var_lst$tab_var <- tab_var
      var_lst$tab_label <- GetVarLabel(x, tab_var)
      var_lst$tab_val_label <- GetValLabels(x, tab_var)
      var_lst$tab_var_num <- length(unique(x[[tab_var]]))

      for (stat in stats) {
        var_lst[getFunctionName(stat)] <-
          list(vals = create_data_tab(x, var, tab_var, extract_lst$tab.max,
                                      stat, na.rm = TRUE))
      }

      # var_lst["groupfreq"] <-
      #   list(vals = create_data_tab(df_tab, var, tab_var, extract_lst$tab.max,
      #                               length))

      if (run_test & (var_lst$tab_var_num > 1)) {
        var_lst$result <-
          oneway.test(as.formula(paste(var, tab_var, sep = " ~ ")), data = x)
        var_lst$result$statsig <- getsig(var_lst$result$p.value)
      } else {
        var_lst$result <- NA
      }

      write_line(extract_lst, var_lst, fileConn)
    }
  }

  # Finish extract -------------------------------------------------------------
  if (!is.na(outputfile)) {
    close(fileConn)
  }

}

# CreateExtractPoints <- function(df, var, tab_vars, notes, outputfile, info,
#                                   stats = c(mean),
#                                   na_action = c("pairwise", "listwise")) {
#
#   if (is.null(na_action) | is.na(na_action)) {
#     na_action <- "listwise"
#   } else if (length(na_action) > 1) {
#     na_action <- "listwise"
#   } else if (purrr::is_empty(which(c("pairwise", "listwise") == na_action))) {
#     na_action <- "listwise"
#   }
#
#   if (!is.na(outputfile)) {
#     check_extracts_dir()
#
#     fileConn <- file(paste("Extracts/", outputfile, ".csv", sep = ""),
#                      open = "wt")
#   } else {
#     fileConn <- NA
#   }
#   browser()
#   if (!purrr::is_empty(info)) {
#     names_info <- stringr::str_replace_all(names(info), ",", ";")
#     info <- stringr::str_replace_all(info, ",", ";")
#     names(info) <- names_info
#   }
#
#   extract_lst <- list(type = "points",
#                       tab_vars = tab_vars,
#                       function_name = "CreateExtractPoints",
#                       info = info,
#                       stats = stats)
#
#
#   if (!purrr::is_empty(tab_vars) & !is.na(tab_vars[[1]])) {
#     ### Nothing here at the moment
#   } else {
#     warning("Makes no sense to run create_extract_points without tab_vars")
#     extract_lst$tabs <- NA
#   }
#
#   extract_lst$run_test <- FALSE
#
#   write_header_line(extract_lst, fileConn)
#
#   # By default include an observation only if not NA for all tab_vars (listwise)
#   # Can also set pairwise deletion where observation is included as long as
#   #   one tab_var is not NA
#   # Observations NA for the variable specified in "var" are always excluded
#   df <- dplyr::mutate(dplyr::select(df, var, tab_vars), nmiss = 0)
#   for (i in c(var, tab_vars)) {
#     df <- dplyr::mutate(df, nmiss = ifelse(!is.na(i), nmiss, nmiss + 1))
#   }
#   if (na_action == "listwise") {
#     df <- dplyr::select(dplyr::filter(df, nmiss == 0), -nmiss)
#   } else if (na_action == "pairwise") {
#     df <- dplyr::filter(df, !is.na(var))
#     df <- dplyr::select(dplyr::filter(df, nmiss < length(tab_vars)), -nmiss)
#   }
#
#
#   write_line(extract_lst, var_lst, fileConn)
#
#   if (!is.na(outputfile)) {
#     close(fileConn)
#   }
# }
