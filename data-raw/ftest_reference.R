#' F.Test data
#'
#' A dataset with 10 values divided in two groups 5 values. The dataset is
#' provided for testing the \code{fct_test} results.
#'
#' @format a dataframe with 5 rows and 2 columns:
#' \describe{
#'    \item{group}{the grouping variable, either \code{a} or \code{b}}
#'    \item{value}{the measured value for the variable of interest}
#' }
#'
#' @name ftest_reference
#' @docType data
#' @source \url{https://support.microsoft.com/en-us/office/f-test-function-100a59e7-4108-46f8-8443-78ffacb6c0a7}
#' @keywords data

ftest_reference <- data.frame(group = c(rep("a", 5), rep("b", 5)),
                              value = c(6, 7, 9, 15, 21, 20, 28, 31, 38, 40))

usethis::use_data(ftest_reference, overwrite = TRUE)
