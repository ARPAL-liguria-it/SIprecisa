#' Tomato yields for two fertilizer mixtures
#'
#' A dataset containing the results expressed in pounds for tomato yields obtained
#'  by a randomized experiment with two fertilizer mixtures. The variable are as
#'  follows:
#'
#' @format a dataframe with 6 rows and 3 columns:
#' \describe{
#'    \item{parameter}{the parameter observed in the experiment, the yield}
#'    \item{fertilizer}{the variable for the useed fertilizer mixture: \code{a} or \code{b}}
#'    \item{pounds}{the measured yields, in pounds}
#' }
#'
#' @name tomato_yields
#' @docType data
#' @author George E. P. Box
#' @author J. Stuart Hunter
#' @author William G. Hunter
#' @source Statistics for Experimenters, Design, Innovation and Discovery.
#'  Wiley, Second Edition, 2005. ISBN: 978-0-471-71813-0.
#'  Section 3.1, pag. 78, table 3.3
#' @keywords data
#' @importFrom data.table fread

tomato_yields <- data.table::fread(system.file("extdata", "raw_tomato_yields", package = "SIconfronta"),
                                   stringsAsFactors = TRUE)

usethis::use_data(tomato_yields, overwrite = TRUE)
