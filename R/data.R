#' Basic earnings and labor force information for recent college graduates
#'
#' A dataset containing basic earnings and labor force information by sex
#' and by the type of job the graduate got. Major categories are based on
#' Carnevale et al, "What's It Worth?: The Economic Value of College Majors."
#' Georgetown University Center on Education and the Workforce, 2011. The
#' dataset was used in a fivethirtyeight.com article titled, "The Economic
#' Guide to Picking a College Major"
#' https://fivethirtyeight.com/features/the-economic-guide-to-picking-a-college-major/
#' and provided through the site's GitHub account.
#'
#' @format A data frame with 173 rows and 19 variables:
#' \describe{
#'   \item{Major_code}{Major code, FO1DP in ACS PUMS}
#'   \item{Major}{Major description}
#'   \item{Total}{Total number of people with major}
#'   \item{Men}{Male graduates}
#'   \item{Women}{Female graduates}
#'   \item{Major_category}{Category of major from Carnevale et al}
#'   \item{Major_category2}{More general category of major used for illustrative purposes}
#'   \item{Sample_size}{Sample size (unweighted) of full-time, year-round ONLY (used for earnings)}
#'   \item{Employed}{Number employed (ESR == 1 or 2)}
#'   \item{Full_time}{Employed 35 hours or more}
#'   \item{Part_time}{Employed less than 35 hours}
#'   \item{Full_time_year_round}{Employed at least 50 weeks (WKW == 1) and at least 35 hours (WKHP >= 35)}
#'   \item{Unemployed}{Number unemployed (ESR == 3)}
#'   \item{Median}{Median earnings of full-time, year-round workers}
#'   \item{P25th}{25th percentile of earnigns}
#'   \item{P75th}{75th percentile of earnings}
#'   \item{College_jobs}{Number with job requiring a college degree}
#'   \item{Non_college_jobs}{Number with job not requiring a college degree}
#'   \item{Low_wage_jobs}{Number in low-wage service jobs}
#' }
#' @source \url{https://github.com/fivethirtyeight/data/tree/master/college-majors}
"college_grads"
