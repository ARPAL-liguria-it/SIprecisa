#' Dissolved orthophosphate concentration values from UNI ISO 11352:2012, Table B.1.
#'
#' A dataset with 30 numerical values. The dataset is
#' provided for testing the \code{fct_precision} results.
#'
#' @format a dataframe with 30 rows and 2 columns:
#' \describe{
#'    \item{id}{a numeric progressive identifier.}
#'    \item{value}{the measured orthophosphate concentration values, expressed as micromoles per liter.}
#' }
#'
#' @name uniiso_11352_b1
#' @docType data
#' @source UNI ISO 11352:2012, Table B.1, pag. 17.
#' \url{https://store.uni.com/iso-11352-2012}
#' @keywords data

uniiso_11352_b1 <- data.frame(id = 1:30,
                              value = c(2.16, 2.40, 2.31, 2.33, 2.36, 2.27, 2.37,
                                        2.27, 2.27, 2.10, 2.26, 2.58, 2.23, 2.47,
                                        2.37, 2.39, 2.30, 2.26, 2.42, 2.67, 2.36,
                                        2.37, 2.36, 2.30, 2.50, 2.17, 2.43, 2.35,
                                        2.16, 2.30))

usethis::use_data(uniiso_11352_b1, overwrite = TRUE)
