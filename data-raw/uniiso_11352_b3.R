#' UNI ISO 11352:2013 Table B.4
#'
#' A dataset with 10 values of relative differences from paired values.
#' The dataset is provided for testing the \code{fct_precision_rip} results.
#'
#' @format a dataframe with 10 rows and 1 column of numeric values named
#' {rel_diff}.
#'
#' @name uniiso_11352_b4
#' @docType data
#' @source \url{https://store.uni.com/iso-11352-2012}
#' @keywords data

uniiso_11352_b4 <- data.frame(rel_diff = c(0.1544, 0.1689, 0.1046, 0.0445, 0.1162,
                                           0.0303, 0.1135, 0.0250, 0.0501, 0.0255))

usethis::use_data(uniiso_11352_b4, overwrite = TRUE)
