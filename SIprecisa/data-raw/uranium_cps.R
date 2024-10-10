#' Mass spectrometer measurements on a Uranium isotope
#'
#' A dataset containing the results expressed counts per seconds for mass spectrometer measurements on a Uranium isotope.
#' The variable are as follows:
#'
#' @format a vector with 8 numerical values:
#'
#' @name uranium_cps
#' @docType data
#' @author Gary L. Tietjen
#' @author Roger H. Moore
#' @source Some Grubbs-Type Statistics for the Detection of Several Outliers.
#'  Technometrics, 14(3), 1972, pp. 583-597.
#'  Also available at
#'  \url{https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h1.htm}.
#' @keywords data

uranium_cps <- c(199.31, 199.53, 200.19, 200.82, 201.92, 201.95, 202.18, 245.57)

usethis::use_data(uranium_cps, overwrite = TRUE)
