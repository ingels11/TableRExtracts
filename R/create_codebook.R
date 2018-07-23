#' Create a Dataset Codebook
#'
#' Create a codebook for a dataset including XXX for all variables.
#'
#' @param df dataframe object to create codebook
#' @param df_file_name name codebook should be saved under
#' @param df_title title printed on first page of codebook
#'
#' @export
CreateCodebook <- function(df, df_file_name, df_title) {

  # First attach shortDescrip to label in dataset and all variables
  df <- CombineAllLabels(df)

  dataMaid::makeDataReport(df,
                           mode = c("summarize", "visualize", "check"),
                           replace = TRUE, smartNum = TRUE,
                           reportTitle = paste0("Codebook for ", df_title),
                           file = paste0(df_file_name, "_codebook", ".RMD"),
                           summaries = dataMaid::setSummaries(character = dataMaid::defaultCharacterSummaries(),
                                                              factor = dataMaid::defaultFactorSummaries(),
                                                              labelled = dataMaid::defaultLabelledSummaries(),
                                                              numeric = dataMaid::defaultNumericSummaries(add = c("meanSummary", "pctZeroes")),
                                                              integer = dataMaid::defaultIntegerSummaries(add = c("meanSummary", "pctZeroes")),
                                                              logical = dataMaid::defaultLogicalSummaries(),
                                                              Date = dataMaid::defaultDateSummaries()))
}
