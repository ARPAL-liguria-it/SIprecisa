#' Observations from UNI ISO 16269-4:2019 - Section 4.3.2
#'
#' A dataset containing 20 observations for outliers detection.
#' The variable are as follows:
#'
#' @format a vector with 20 numerical values:
#'
#' @name uniiso_16269_4_432
#' @docType data
#' @author ISO/TC 69 - Applications of statistical methods
#' @source UNI ISO 16269-4:2019 - Statistical interpretation of data - Part 4:
#'  Detection and treatment of outliers. Section 4.3.2.
#'  \url{https://store.uni.com/uni-iso-16269-4-2019}.
#' @keywords data

uniiso_16269_4_432 <- c(-2.21, -1.84, -0.95, -0.91, -0.36,
                        -0.19, -0.11, -0.10, 0.18, 0.30,
                        0.43, 0.51, 0.64, 0.67, 0.93,
                        1.22, 1.35, 1.73,5.80, 12.60)

usethis::use_data(uniiso_16269_4_432, overwrite = TRUE)
