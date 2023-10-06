#' UNI ISO 2854:1988 Prospetto X
#'
#' A dataset with 22 values divided in two groups with 10 and 12 values, respectively.
#' The dataset is' provided for testing the \code{fct_test} results.
#'
#' @format a dataframe with 12 rows and 2 columns:
#' \describe{
#'    \item{group}{the grouping variable, either \code{a} or \code{b}}
#'    \item{value}{the breaking load numerical values for the tested yarns, expressed in Newton.}
#' }
#'
#' @name uniiso_2854_x
#' @docType data
#' @source \url{https://store.uni.com/uni-iso-2854-1988}
#' @keywords data

uniiso_2854_x <- data.frame(group = c(rep("a", 10), rep("b", 12)),
                              value = c(2.297, 2.582, 1.949, 2.362, 2.040, 2.133, 1.855, 1.986, 1.642, 2.915,
                                        2.286, 2.327, 2.388, 3.172, 3.158, 2.751, 2.222, 2.367, 2.247, 2.512, 2.104, 2.707))

usethis::use_data(uniiso_2854_x, overwrite = TRUE)
