#' Create a Dataset Codebook
#'
#' Create a codebook for a dataset including XXX for all variables.
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
                                                              numeric = dataMaid::defaultNumericSummaries(add = "meanSummary"),
                                                              integer = dataMaid::defaultIntegerSummaries(add = "meanSummary"),
                                                              logical = dataMaid::defaultLogicalSummaries(),
                                                              Date = dataMaid::defaultDateSummaries()))
}
