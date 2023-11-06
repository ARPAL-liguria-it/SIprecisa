#' Plotly boxplots for a differences of paired measurements.
#'
#' @description The function provides a simple {plotly} boxplot for differences
#' of paired measurement values.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *rel_response* with the numeric values of the relative difference
#' between the paired values, and a column named *outlier* with a logical vector.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {plotly} boxplot for measurement values.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers layout config
boxplot_rip <- function(data,
                        response,
                        udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(udm),
    c("key", "outlier", "rel_response") %in% colnames(data),
    dim(data)[2] >= 3
  )

  cols <- ifelse(data$outlier == TRUE,
                 "#999999",
                 "black")

  ylabtitle <- paste0("differenze relative (", udm, ")")

  datanoutlier <- data[data$outlier == FALSE,]

  # boxplot for measurement values
  plotly::plot_ly(source = "boxplot") |>
    plotly::add_boxplot(
      data = datanoutlier,
      y = ~ rel_response * 100,
      x = "differenze",
      name = "boxplot",
      type = "box",
      boxmean = TRUE,
      boxpoints = FALSE,
      color = I("#2780E3"),
      showlegend = FALSE,
      whiskerwidth = 0,
      key = NULL
    ) |>
    plotly::add_markers(
      data = data,
      y = ~ rel_response * 100,
      x = "differenze",
      name = "differenze",
      marker = list(
        color = I(cols),
        colors = I(cols),
        size = 10
      ),
      key = ~ key,
      hoverinfo = "y",
      hovertemplate = paste('%{y:.3r}', udm)
    ) |>
    plotly::layout(
      showlegend = FALSE,
      yaxis = list(title = ylabtitle,
                   hoverformat = ".3r")
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' ggplot2 boxplots for paired measurement values.
#'
#' @description The function provides a simple {ggplot2} boxplot for a serie
#'  of data, and for comparing the confidence interval with a reference value
#'  and its extended uncertainty.
#'
#' @param data input data.frame a numeric columns with the two numeric responses and a
#' character columns with outliers flag.
#' The first two columns must be named as the response and second_response variable,
#' respectively, while the second column must be named *rimosso*.
#' @param response a character string with the label for the first response numeric variable.
#' @param second_response a character string with the label for the second response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot for the differences in paired measurement values.
#'
#' @export
#'
#' @import patchwork
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_rip <- function(data,
                          response,
                          second_response) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(second_response)
  )

  rimosso <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")
  data$rimosso <- factor(data$rimosso, levels = c("s\u00EC", "no"))
  data$difference <- data[[response]] - data[[second_response]]
  data$mean <- data[, rowMeans(.SD),
                    .SDcols = c(response, second_response)]
  data$rel_diff <- data$difference / data$mean

  datanoutlier <- data[which(data$rimosso == "no"),]

  ylabtitle <- "differenze relative (%)"

  myboxplot <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = datanoutlier,
                          ggplot2::aes(x = "differenze",
                                       y = rel_diff * 100),
                          fill = "white",
                          col = "black",
                          outlier.shape = NA) +
    ggplot2::geom_jitter(data = data,
                         ggplot2::aes(x = "differenze",
                                      y = rel_diff * 100,
                                      col = rimosso),
                         width = 0.2) +
    ggplot2::labs(x = ggplot2::element_blank(),
                  y = ylabtitle,
                  title = "Boxplot delle differenze relative tra coppie di valori") +
    ggplot2::scale_color_manual(values = cols,
                                breaks = c("s\u00EC", "no"),
                                labels = c("rimosso", "non rimosso"),
                                name = ggplot2::element_blank(),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  myboxplot

}

#' Summary arranged on rows for a set of measurements and a reference value
#'
#' @description The function returns a table with max, mean, median, min, n and
#'  the number of removed points.
#'  values are arranged on rows, columns are for measurement and reference values,
#'  respectively . Numbers are formatted as text in order to provide the
#'  desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised a
#' column named *outlier* with a logical flag must be included.
#' @param refuncertainty a number with the extended uncertainty for the reference value.
#' @param udm a string with the unit of measurement.
#' @param signif an integer with the number of desired significant figures.
#'
#' @return a \code{data.table} with 6 rows and \eqn{n + 1} columns for \eqn{n}
#'   levels of the group variable.
#'
#' @export
#'
#' @import data.table
#' @importFrom stats sd median
rowsummary_rip <- function(data,
                           response,
                           udm = "",
                           signif = 3L) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(udm),
    all.equal(signif, as.integer(signif)),
    response %in% colnames(data),
    "outlier" %in% colnames(data)
  )

  mydata <- data.table::data.table(data)
  myrows <- c("n esclusi", "n", "massimo", "media", "mediana", "minimo")

  # calculate the summary
  mysummary <- mydata[outlier == FALSE, .(
    n = .N,
    massimo = lapply(.SD, max, na.rm = TRUE) |> unlist() |> format_sigfig(signif),
    media = lapply(.SD, mean, na.rm = TRUE) |> unlist() |> format_sigfig(signif),
    mediana = lapply(.SD, stats::median, na.rm = TRUE) |> unlist() |> format_sigfig(signif),
    minimo = lapply(.SD, min, na.rm = TRUE) |> unlist() |> format_sigfig(signif)
    ),
    .SDcols = response
  ] |> data.table::transpose()

  n_out <- mydata[outlier == TRUE, .N]

  thesummary <- data.table::data.table(statistica = myrows,
                                       differenze = c(n_out, mysummary$V1))

  thesummary$differenze <- ifelse(thesummary$statistica %notin% c("n esclusi", "n"),
                                  paste0(thesummary$differenze, " ", udm),
                                  thesummary$differenze)

  thesummary
}

#' Displays the results for the determination of precision performances calculated
#' from paired measurement values.
#'
#' @description The function displays the results for the determination
#' of precision performances of an analytical method, based on a set of paired
#' measurement values.
#' The returned text is suitable for the {SI precisa} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. At least a \code{numeric} vector with the
#'   measurements should be included.
#' @param response the name of a numeric vector in \code{data} with the
#' relative absolute differences between paired measurement values.
#' @param significance the level of significance for the repeatability limit
#' calculation. Typical values are 0.90, 0.95 or 0.99.
#'
#' @details precision expressed as standard deviations, repeatability limit
#' and relative standard deviation (rsd) are calculated according to the Eurachem
#' guide - The Fitness for Purpose of Analytical Methods.
#'
#' @return A list with the following items:
#'  \describe{
#'    \item{alpha}{a numeric value calculated as significance + (1 - significance)/2.}
#'    \item{n}{the number of measurements.}
#'    \item{mean}{a numeric value with the mean of the values.}
#'    \item{rsd}{a numeric value with the relative standard deviation.}
#'    \item{repeatability}{a numeric value with the repeatability limit.}
#'  }
#'
#' @export
#'
#' @importFrom stats sd qt
fct_precision_rip <- function(data,
                              response,
                              significance = 0.95) {

  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.numeric(significance),
    response %in% colnames(data)
  )



  myalpha <- (significance + (1 - significance) / 2)

  mymean <- data[[response]] |> mean()

  rsd <- 100 * mymean / 1.128

  n <- length(data[[response]])

  relative_repeatability <- (sqrt(2) * stats::qt(myalpha, n - 1) * rsd)

  list(
    alpha = myalpha,
    n = n,
    mean = mymean,
    rsd = rsd,
    rel_repeatability = relative_repeatability
  )

}
