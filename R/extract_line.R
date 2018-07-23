write_header_line <- function(extract_lst, connection = NA) {

  x1 <- c()
  if (extract_lst$type == "frequency" | extract_lst$type == "mean") {
    x1 <- c("UniqueId")
  }
  x1 <- c(x1, "Variable", "VarLabel")
  if (extract_lst$type == "frequency") {
    x1 <- c(x1, "Value", "ValLabel")
  }
  tab_seq <- seq.int(from = 1, to = extract_lst$tab.max, by = 1)
  if (extract_lst$type == "frequency" | extract_lst$type == "mean") {
    if (!is.na(extract_lst$tab_vars)[[1]]) {
      x1 <- c(x1, "TabVar", "TabLabel", paste("TabVal", tab_seq, sep = "_"))
    }
  } else if (extract_lst$type == "points") {
    x1 <- c(x1, "TabVars")
  }

  if (extract_lst$type == "frequency") {
    if (is.na(extract_lst$tab_vars)[[1]]) {
      x2 <- c("Percent", "Frequency", "TotFreq", "Missing", "NObs")
    } else {
      x2 <- c(paste("Percent", tab_seq, sep = "_"),
              "Percent", paste("Frequency", tab_seq, sep = "_"),
              "Frequency", paste("GroupFreq", tab_seq, sep = "_"),
              "TotFreq", "Missing", "NObs")

    }
  } else if (extract_lst$type == "mean") {
    x2 <- c()
    for (stat in extract_lst$stats) {

      x2 <- c(x2, paste(stringr::str_to_title(getFunctionName(stat)),
                        tab_seq, sep = "_"),
              stringr::str_to_title(getFunctionName(stat)))
    }
    # Add in the frequency variables
    x2 <- c(x2, paste("GroupFreq", tab_seq, sep = "_"), "TotFreq",
            "Missing", "NObs")
  } else if (extract_lst$type == "points") {
    ### need to add frequency related stuff here
    x2 <- c("TotFreq", "NObs")
  }
  if (extract_lst$run_test) {
    x2 <- c(x2, "Statistic", "Parameter", "PVal", "Method", "StatSig")
  }

  x3 <- c("Dataset", "Dataset_Label")
  if (!purrr::is_empty(extract_lst$info)) {
    x3 <- c(x3, names(extract_lst$info))
  }
  x3 <- c(x3, "FunctionName", "ProgDate", "ProgTime")
  if (is.na(extract_lst$notes) | is.null(extract_lst$notes)) {
    x3 <- c(x3, "Notes")
  }

  if (is.na(connection)) {
    print(paste(c(x1, x2, x3), collapse = ","))
  } else {
    writeLines(c(x1, x2, x3),
               con = connection, sep = ",")
    writeLines("", con = connection)
    #return(NULL)
  }

}

write_line <- function(extract_lst, line_lst, connection = NA) {

  if (extract_lst$type == "frequency") {
    # UniqueId
    x1 <- paste(line_lst$var_name, line_lst$val, sep = ":")
  } else if (extract_lst$type == "mean") {
    # UniqueId
    x1 <- paste(line_lst$var_name, line_lst$tab_var, sep = ":")
  }

  # Variable, VarLabel
  x2 <- c(line_lst$var_name, line_lst$var_label)
  if (extract_lst$type == "frequency") {
    # Value, ValLabel
    x2 <- c(x2, line_lst$val, line_lst$val_label)
  }
  if (!is.na(line_lst$tab_var)) {
    # TabVar, TabLabel
    x2 <- c(x2, line_lst$tab_var, line_lst$tab_label)
    for (i in 1:extract_lst$tab.max) {
      if (i <= length(line_lst$tab_val_label$labs)) {
        # TabVal_
        x2 <- c(x2, line_lst$tab_val_label$labs[i])
      } else {
        # TabVal_
        x2 <- c(x2, "")
      }
    }
  }

  if (extract_lst$type == "frequency") {
    # Percent_, Percent, Frequency_, Frequency,
    # GroupFreq_, TotFreq, Missing, Nobs
    x3 <- toString(c(line_lst$val_freq / line_lst$var_freq,
                   line_lst$val_freq, line_lst$var_freq,
                   line_lst$nobs - line_lst$var_freq[["Overall"]],
                   line_lst$nobs))

  } else if (extract_lst$type == "mean") {
    x3 <- c()
    for (i in extract_lst$stats) {
      x3 <- c(x3, toString(line_lst[[getFunctionName(i)]]))
    }

    # GroupFreq_, TotFreq, Missing, Nobs
    x3 <- c(x3, toString(c(line_lst$var_freq,
            line_lst$nobs - line_lst$var_freq[["Overall"]], line_lst$nobs)))
  }

  if (extract_lst$run_test) {
    # Statistic, Parameter, PVal, Method, StatSig
    if (is.na(line_lst$result)[[1]]) {
      x3 <- c(x3, rep("", 5))
    } else {
      x3 <- c(x3, toString(c(line_lst$result$statistic,
                             line_lst$result$parameter[[1]],
                             line_lst$result$p.value,
                             line_lst$result$method,
                             line_lst$result$statsig)))
    }

  }

  x4 <- c(extract_lst$dataset, extract_lst$dataset_label)
  if (!purrr::is_empty(extract_lst$info)) {
    # Varies by user, commonly: FileName, DirName, Programmer
    x4 <- c(x4, extract_lst$info)
  }
  # FunctionName, ProgDate, ProgTime
  x4 <- c(x4, extract_lst$function_name, GetDateTime())

  if (is.na(connection)) {
    print(paste(c(x1, x2, x3, x4), collapse = ","))
  } else {
    writeLines(c(x1, x2, x3, x4),
               con = connection, sep = ",")
    writeLines("", con = connection)
    #return(NULL)
  }

}
