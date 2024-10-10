#' Shapiro-Wilk test data
#'
#' A dataset with 11 values of men weights expressed in pounds.
#' The dataset is provided for testing the results of \code{fct_shapiro}.
#'
#' @format a vector with 11 numerical elements.
#'
#' @name shapiro_reference
#' @docType data
#' @author S. S. Shapiro
#' @author M. B. Wilk
#' @source An analysis of variance test for normality (complete samples),
#'  Biometrika (1965), 52, 3 and 2, p. 591.
#'  Section 4 - Examples, pag. 606, Example 1.
#'  \url{http://links.jstor.org/sici?sici=0006-3444%28196512%2952%3A3%2F4%3C591%3AAAOVTF%3E2.0.CO%3B2-B}
#' @keywords data

shapirotest_reference <- c(148, 154, 158, 160, 161, 162, 166, 170, 182, 195, 236)

usethis::use_data(shapirotest_reference, overwrite = TRUE)
