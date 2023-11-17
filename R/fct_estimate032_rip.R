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
    c("key", "outlier", response) %in% colnames(data),
    dim(data)[2] >= 3
  )

  cols <- ifelse(data$outlier == TRUE,
                 "#999999",
                 "black")

  ylabtitle <- paste0("differenza relativa (", udm, ")")

  datanoutlier <- data[data$outlier == FALSE,]
  datanoutlier$myresponse <- datanoutlier[[response]]

  mydata <- data
  mydata$myresponse <- data[[response]]

  # boxplot for measurement values
  plotly::plot_ly(source = "boxplot") |>
    plotly::add_boxplot(
      data = datanoutlier,
      y = ~ myresponse * 100,
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
      data = mydata,
      y = ~ myresponse * 100,
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

#' Plotly widget for differences between pair of values vs their median values.
#'
#' @description The function provides a simple {plotly} for
#' differences between pair of values vs their mean values.
#'
#' @details
#' relative percentage difference between values are plotted vs the mean of the
#' paired values. The central line is calculated as the mean of the relative
#' differences and the upper limit is the calculated by multiplying the value of
#' the central line by 3.267, as described in section 6.4 of UNI ISO 7870-2:2023.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' two columns named *measure1* and *measure2* with the numeric values
#' of the paired measurement values, and a column named *outlier* with a logical vector.
#' @param measure1 a character string with the label for the first response
#' numeric variable.
#' @param measure2 a character string with the label for the second response
#' numeric variable.
#' @param udm a character string with unit of measurement of the paired values.
#' Default is an empty string.
#'
#' @return A {plotly} interval plot for comparing the confidence
#' interval of the supplied  measurement values with a reference value and
#' its extended uncertainty.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_markers layout config add_annotations
shewart_rip <- function(data,
                        measure1,
                        measure2,
                        udm = "") {
  stopifnot(
    is.data.frame(data),
    is.character(measure1),
    is.character(measure2),
    is.character(udm),
    c("key", "outlier", measure1, measure2) %in% colnames(data),
    dim(data)[2] >= 4
  )


  cols <- ifelse(data$outlier == TRUE,
                 "#999999",
                 "black")

  myudm <- ifelse(udm == "", "", paste0(" (", udm, ")"))
  xlabtitle <- paste0("Valore medio", myudm)

  datanoutlier <- data[data$outlier == FALSE,]

  # limits for the mean(x) vs relative range chart
  datanoutlier$myranges <- abs(datanoutlier[[measure1]] - datanoutlier[[measure2]])
  datanoutlier$mymean <- rowMeans(datanoutlier[c(measure1, measure2)])
  datanoutlier$perc_ranges <- 100 * datanoutlier$myranges / datanoutlier$mymean

  perc_ranges_mean <- datanoutlier$perc_ranges |> mean()
  perc_ranges_limit <- perc_ranges_mean * 3.267

  # getting relative ranges for all data points
  mydata <- data
  mydata$myranges <- abs(mydata[[measure1]] - mydata[[measure2]])
  mydata$mymean <- rowMeans(mydata[c(measure1, measure2)])
  mydata$perc_ranges <- 100 * mydata$myranges / mydata$mymean

  # comparing confidence intervals
  plotly::plot_ly(source = "boxplot",
                  data = mydata,
                  y = ~perc_ranges ,
                  x = ~mymean,
                  name = "differenza",
                  type = "scatter",
                  mode = "markers",
                  showlegend = FALSE,
                  key = ~ key,
                  hovertemplate = paste('%{y:.3r}', "%"),
                  marker = list(
                    color = I(cols),
                    colors = I(cols),
                    size = 10
                  )
  ) |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(title = xlabtitle),
      yaxis = list(title = "| differenza relativa | (%)",
                   hoverformat = ".3r"),
      shapes = list(hline(perc_ranges_mean, "#2780E3", dash = "dash"),
                    hline(perc_ranges_limit, "#d62728", dash = "dash"))
    ) |>
    plotly::add_annotations(x = 0,
                            y = perc_ranges_mean,
                            xref = "paper",
                            yref = "y",
                            text = "Media",
                            xanchor = 'left',
                            yanchor = "bottom",
                            showarrow = FALSE) |>
    plotly::add_annotations(x = 0,
                            y = perc_ranges_limit,
                            xref = "paper",
                            yref = "y",
                            text = "Limite",
                            xanchor = 'left',
                            yanchor = "bottom",
                            showarrow = FALSE) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' ggplot2 boxplots for paired measurement values.
#'
#' @description The function provides a simple {ggplot2} boxplot for a serie
#'  of paired measurement values, and a scatter plot of the absolute difference
#'  between paired data vs their mean values.
#'
#' @details
#' In the second panel of the plot, relative percentage difference between values
#' are plotted vs the mean of the paired values.
#' The central line is calculated as the mean of the relative
#' differences and the upper limit is the calculated by multiplying the value of
#' the central line by 3.267, as described in section 6.4 of UNI ISO 7870-2:2023.
#'
#'
#' @param data input data.frame a numeric columns with the two numeric responses and a
#' character columns with outliers flag.
#' The first two columns must be named as the response and second_response variable,
#' respectively, while the third column must be named *rimosso*.
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
                          second_response,
                          udm = "") {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(second_response),
    "rimosso" %in% colnames(data),
    is.character(udm)
  )

  rimosso <- NULL
  rel_diff <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")
  data$rimosso <- factor(data$rimosso, levels = c("s\u00EC", "no"))
  data$difference <- data[[response]] - data[[second_response]]
  data$mean <- data[, rowMeans(.SD),
                    .SDcols = c(response, second_response)]
  data$rel_diff <- data$difference / data$mean

  datanoutlier <- data[which(data$rimosso == "no"),]

  ylabtitle1 <- "differenza relativa (%)"
  ylabtitle2 <- "| differenza relativa | (%)"
  myudm <- ifelse(udm == "", "", paste0(" (", udm, ")"))
  xlabtitle2 <- paste0("Valore medio", myudm)

  minmean <- data$mean |> min()
  mean_range <- 100 * datanoutlier$rel_diff |> abs() |> mean()
  limit_range <- mean_range * 3.267

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
                  y = ylabtitle1,
                  title = "Boxplot") +
    ggplot2::scale_color_manual(values = cols,
                                breaks = c("s\u00EC", "no"),
                                labels = c("rimosso", "non rimosso"),
                                name = ggplot2::element_blank(),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  myrchart <- ggplot2::ggplot() +
    ggplot2::geom_point(data = data,
                        ggplot2::aes(x = mean,
                                     y = 100 * abs(rel_diff),
                                     col = rimosso)) +
    ggplot2::geom_hline(yintercept = mean_range,
                        color = "#999999",
                        linetype = "dashed") +
    ggplot2::geom_hline(yintercept = limit_range,
                        color = "black",
                        linetype = "dashed") +
    ggplot2::annotate("text",
                      x = minmean,
                      y = mean_range,
                      label = "Media",
                      vjust = 1.4,
                      hjust = -0.1) +
    ggplot2::annotate("text",
                      x = minmean,
                      y = limit_range,
                      label = "Limite",
                      vjust = 1.4,
                      hjust = -0.1) +
    ggplot2::labs(x = xlabtitle2,
                  y = ylabtitle2,
                  title = "R chart") +
    ggplot2::scale_color_manual(values = cols,
                                breaks = c("s\u00EC", "no"),
                                labels = c("rimosso", "non rimosso"),
                                name = ggplot2::element_blank(),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")


  myboxplot + myrchart +
    patchwork::plot_layout(widths = c(3, 4), nrow = 1)

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
#' @param response a character with the name of the data columns with the measured values.
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

  outlier <- NULL
  mydata <- data.table::data.table(data)
  myrows <- c("n esclusi", "n", "massimo", "media", "mediana", "minimo")

  # calculate the summary
  mysummary <- mydata[outlier == FALSE, list(
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

  colnames(thesummary) <- c("statistica", "| differenza relativa |")

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
