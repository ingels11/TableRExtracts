####
# Basic setup of extract lines
# UniqueId: Combination of index variable name and tabulating variable
# Variable: Index variable
# VarLabel: Character label for index variable
# (Optional) TabVar: Tabulating variable
# (Optional) TabVarLabel: Character label for tabulating variable
# (Optional) StratVar: Stratifying variable
# Possible statistics
#   * Frequency
#   * Proportion
#   * (Optional) Mean, Median, Max, Min, Median, First Quartile, Third Quartile
#                Standard Deviation, Variance, Correlation, Mode, Kurtosis,
#                Skewness, Covariance
#   *
# Missing
# Nobs
# Test Statistics
# ProgDir, ProgName, Programmer, FunctionName, ProgDate, ProgTime, Notes

GetExtractLine <- function(type, lst, subtype = NA, header = FALSE) {

  if (header) {
    if (type == "mean") {
      x1 <- c("UniqueId", "Variable", "VarLabel", "TabVar", "TabVarLabel",
              "StratVar",
              lst$mean.vec,  "Mean", lst$var.vec, "Var",
              lst$median.vec, "Median",
              lst$min.vec, "Min", lst$max.vec, "Max",
              lst$q1.vec, "Quartile1", lst$q3.vec, "Quartile3",
              lst$group.vec, "TotFreq",	"Missing",	"NObs", "Weights",
              lst$posthoc.vec,
              "ZValue",	"ZProb", "ZTest", "ConfInt",	"NumTabVals",
              "TabFormat", "SigLevel")
    } else if (type == "freq") {
      if (is.na(lst$group.vec)) {
        x1 <- c("UniqueId", "Variable", "VarLabel", "Value", "ValLabel",
                lst$colpct.vec, lst$freq.vec,
                "TotFreq",	"Missing",	"NObs")
      } else {
        x1 <- c("UniqueId", "Variable", "VarLabel", "Value",
                lst$colpct.vec, lst$freq.vec, lst$group.vec,
                "TotFreq",	"Missing",	"NObs",	"TestStat",
                "TestProb",	"Test", "TabFormat",	"SigLevel")
      }

    } else if (type == "twopart") {
      x1 <- c("UniqueId", "Variable", "VarLabel", "TabVar", "TabVarLabel",
              "StratVar",
              lst$mean.vec,  "Mean", lst$var.vec, "Var",
              lst$median.vec, "Median",
              lst$min.vec, "Min", lst$max.vec, "Max",
              lst$q1.vec, "Quartile1", lst$q3.vec, "Quartile3",
              lst$group.vec, "TotFreq",	"Missing",	"NObs",
              "2Part_Val1", "2Part_Val2", "2Part_Coef",
              "2Part_CI_Level", "2Part_CI_Lower", "2Part_CI_Upper")
    }
    x2 <- c("ProgDir",	"ProgName",	"Programmer",
            "FunctionName", "ProgDate",	"ProgTime", "Notes")
    return(c(x1, x2))
  }
}
