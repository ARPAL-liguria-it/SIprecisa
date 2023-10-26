#' Plotly boxplots for a differences of paired measurements.
#'
#' @description The function provides a simple {plotly} boxplot for differences
#' of paired measurement values.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *response* with the numeric values for the two groups and a
#' column named *outlier* with a logical vector.
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
    c("key", "outlier", "response") %in% colnames(data),
    dim(data)[2] >= 3
  )

  cols <- ifelse(data$outlier == TRUE,
                 "#999999",
                 "black")

  ylabtitle <- paste0("differenze",
                      ifelse(udm != "", paste0(" (", udm, ")"), ""))

  datanoutlier <- data[data$outlier == FALSE,]

  # boxplot for measurement values
  plotly::plot_ly(source = "boxplot") |>
    plotly::add_boxplot(
      data = datanoutlier,
      y = ~ response,
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
      y = ~ response,
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
      title = list(text = "Boxplot delle differenze",
                   font = list(size = 11)
      ),
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
#' @param data input data.frame a numeric columns with the response and a
#' character columns with outliers flag.
#' The first column must be named as the response variable, while the second column
#' must be named *rimosso*.
#' @param response a character string with the label for the response numeric variable.
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
                          udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(udm)
  )

  rimosso <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")
  data$rimosso <- factor(data$rimosso, levels = c("s\u00EC", "no"))

  datanoutlier <- data[which(data$rimosso == "no"),]

  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  quo_response <- ggplot2::ensym(response)

  myboxplot <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = datanoutlier,
                          ggplot2::aes(x = "differenze",
                                       y = !!quo_response),
                          fill = "white",
                          col = "black",
                          outlier.shape = NA) +
    ggplot2::geom_jitter(data = data,
                         ggplot2::aes(x = "differenze",
                                      y = !!quo_response,
                                      col = rimosso),
                         width = 0.2) +
    ggplot2::labs(x = ggplot2::element_blank(),
                  y = ylabtitle,
                  title = "Boxplot delle differenze tra coppie di valori") +
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
      massimo = max(response, na.rm = TRUE) |> format_sigfig(signif),
      media = mean(response, na.rm = TRUE) |> format_sigfig(signif),
      mediana = stats::median(response, na.rm = TRUE) |> format_sigfig(signif),
      minimo = min(response, na.rm = TRUE) |> format_sigfig(signif)
  )] |> data.table::transpose()

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
#' differences between paired measurement values.
#' @param measures the name of the two numeric vector in \code{data} with
#' the measured values.
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
#'    \item{stddev}{a numeric value with the standard deviation of the values.}
#'    \item{repeatability}{a numeric value with the repeatability limit.}
#'    \item{rsd}{a numeric value with the relative standard deviation.}
#'  }
#'
#' @export
#'
#' @importFrom stats sd qt
fct_precision_rip <- function(data,
                              response,
                              measures,
                              significance = 0.95) {

  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.numeric(significance),
    is.character(measures),
    length(measures) == 2,
    response %in% colnames(data)
  )



  myalpha <- (significance + (1 - significance) / 2)

  pair_range <- data[[response]] |> abs()
  pair_mean <- data[measures] |> rowMeans()

  rsd <- (pair_range / pair_mean) |> mean()

  n <- length(data[[response]])

  relative_repeatability <- (sqrt(2) * stats::qt(myalpha, n - 1) * rsd)

  list(
    alpha = myalpha,
    n = n,
    rel_repeatability = relative_repeatability * 100,
    rsd = rsd * 100
  )

}
