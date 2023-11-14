#' Manganese mean concentration values from UNI ISO 5725-4:2020, Table B.3.
#'
#' A dataset with 12 numerical values. The dataset is
#' provided for testing the \code{fct_precision} and \code{fct_trueness} results.
#'
#' @format a dataframe with 12 rows and 2 columns:
#' \describe{
#'    \item{lab}{a numeric value for the mean value for each laboratory measurements.}
#'    \item{value}{the percentage mean of the measured manganese contents provided by each laboratory}
#' }
#'
#' @name uniiso_5725_4_b3
#' @docType data
#' @source UNI ISO 5725-4:2020, Table B.3, Content level 3, pag. 20.
#' \url{https://store.uni.com/uni-iso-5725-4-2020}
#' @keywords data

uniiso_5725_4_b3 <- data.frame(lab = 1:12,
                             value = c(0.4143, 0.4006, 0.4004, 0.4072, 0.3959, 0.4049,
                                       0.3850, 0.3947, 0.4023, 0.4043, 0.4077, 0.4077))

usethis::use_data(uniiso_5725_4_b3, overwrite = TRUE)
