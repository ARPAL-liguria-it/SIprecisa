#' Displays the results of an \eqn{E_n}-test for two values with
#' extended uncertainties
#'
#' @description The function displays the results of a \eqn{E_n}-test for two
#' values with extended uncertainty.
#'  The returned text is suitable for the {SI precisa} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. Two \code{numeric} vectors with the measurements
#'   values and their uncertainties should be included.
#' @param response the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param uncertainty the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param refvalue a \code{numeric} reference value.
#' @param refuncertainty the \code{numeric} uncertainty of the reference value.
#'
#' @details \eqn{E_n}-test is calculated with the following equation:
#' \deqn{E_n = \frac{|x - y|}{\sqrt{U(x)^2 + U(y)^2}}}
#' for \eqn{E_n \leq 1}, the difference of the two values \eqn{x} and \eqn{y} is
#' accounted by their extended uncertainties (\eqn{U}) and the null hypothesis is
#' not rejected. When \eqn{E_n > 1} the two values are different and the null
#' hypothesis is rejected.
#'
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{difference}{a named vector of numbers, being \code{mean},
#'    \code{lwrci} and \code{uprci} the mean, lower and upper ends of the
#'    confidence interval for the difference of the two values, respectively.
#'    The confidence interval is calculated considered the combined extended
#'    uncertainties of the two values.}
#'    \item{test}{a named vector of numbers, being \code{entest} and
#'    \code{ecritical} the value of the test statistic and the associated critical
#'    value, respectively.}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
fct_entest_recuno <- function(data,
                              response,
                              uncertainty,
                              refvalue,
                              refuncertainty) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(uncertainty),
    is.numeric(refvalue),
    is.numeric(refuncertainty),
    response %in% colnames(data)
  )

  max_val_lbl <- ifelse(refvalue > data[[response]], "valore di riferimento", "valore misurato")
  min_val_lbl <- ifelse(refvalue < data[[response]], "valore di riferimento", "valore misurato")

  max_val_num <- ifelse(refvalue > data[[response]], refvalue, data[[response]])
  min_val_num <- ifelse(refvalue < data[[response]], refvalue, data[[response]])
  max_val_unc <- ifelse(refuncertainty > data[[uncertainty]], refvalue, data[[uncertainty]])
  min_val_unc <- ifelse(refuncertainty < data[[uncertainty]], refvalue, data[[uncertainty]])

  # en-test results
  diff_val <- (max_val_num - min_val_num)
  diff_unc <- sqrt(max_val_unc^2 + min_val_unc^2)
  diff_confint <- diff_val + c(-1, +1) * diff_unc
  entest <- diff_val / diff_unc
  ecritical <- 1

  # Being clear with some text
  h0_text <- sprintf("%s = %s", max_val_lbl, min_val_lbl)
  h1_text <- sprintf("%s \u2260 %s", max_val_lbl, min_val_lbl)

  positive <- sprintf("%s e %s sono differenti", max_val_lbl, min_val_lbl)
  negative <- sprintf("%s e %s non sono statisticamente differenti", max_val_lbl, min_val_lbl)

  result <- ifelse(entest < ecritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       difference = c("mean" = diff_val |> format_sigfig(),
                      "lwrci" = diff_confint[1] |> format_sigfig(),
                      "uprci" = diff_confint[2] |> format_sigfig()),
       test = c("entest" = entest |> format_sigfig(),
                "ecritical" = ecritical |> format_sigfig()),
       result = result)
  }


#' Plotly boxplots for comparing two values with extended uncertainties
#'
#' @description The function provides a simple {plotly} boxplot for comparing
#' two values with extended uncertainties.
#'
#' @param data input data.frame with a column named *response* with the numeric values
#' and a column named *outlier* with a logical vector.
#' @param response a character string with the label for the response numeric variable.
#' @param uncertainty a character string with the label for the uncertainty numeric variable.
#' @param refvalue a \code{numeric} reference value.
#' @param refuncertainty the \code{numeric} uncertainty of the reference value.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {plotly} boxplot for comparing a group of values with a reference value.
#' Raw data values are overlaid on top of the box.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers add_lines layout config
boxplot_recuno <- function(data,
                           response,
                           uncertainty,
                           refvalue,
                           refuncertainty,
                           udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(uncertainty),
    is.numeric(refvalue),
    is.numeric(refuncertainty),
    is.character(udm),
    colnames(data) %in% c("response", "uncertainty")
  )

  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  meas_val_lbl <- "valore misurato"
  ref_val_lbl <- "valore di riferimento"

  meas_val <- data$response
  meas_unc <- data$uncertainty

  mydf <- data.frame(label = c(meas_val_lbl, ref_val_lbl),
                     value = c(meas_val, refvalue),
                     min_error = if (meas_val > refvalue) c(meas_unc, 0) else c(0, refuncertainty),
                     max_error = if (meas_val > refvalue) c(0, refuncertainty) else c(meas_unc, 0))

  plotly::plot_ly(source = "en") |>
    plotly::add_markers(
      data = mydf,
      y = ~ value,
      x = ~ label,
      error_y = ~ list(symmetric = FALSE,
                       arrayminus = min_error,
                       array = max_error),
      marker = list(
        size = 30,
        symbol = "square"
        ),
      name = "valori",
      color = I("#2780E3"),
      showlegend = FALSE,
      hoverinfo = "y",
      hovertemplate = paste('%{y:.3s}', udm)
    ) |>
    plotly::layout(
      showlegend = FALSE,
      title = NULL,
      xaxis = list(title = NA,
                   categoryorder = "array",
                   categoryarray = c("valore misurato",
                                     "valore di riferimento")),
      yaxis = list(title = ylabtitle,
                   hoverformat = ".3s")
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' GGplot2 boxplots for comparing two values with extended uncertainties
#'
#' @description The function provides a simple {ggplot2} boxplot for comparing
#' two values with extended uncertainties.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column with the numeric values for the two groups and a column named *rimosso*
#' with "sì" or "no" values.
#' @param response a character string with the label for the response numeric variable.
#' @param uncertainty a character string with the label for the uncertainty numeric variable.
#' @param refvalue a \code{numeric} reference value.
#' @param refuncertainty the \code{numeric} uncertainty of the reference value.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot for comparing two values with extended uncertainties.
#' Raw data values are overlaid on top of the box.
#'
#' @export
#'
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_recuno <- function(data,
                             response,
                             uncertainty,
                             refvalue,
                             refuncertainty,
                             udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(uncertainty),
    is.numeric(refvalue),
    is.numeric(refuncertainty),
    is.character(udm)
  )

  min_error <- NULL
  max_error <- NULL
  label <- NULL
  value <- NULL

  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  max_val_lbl <- ifelse(refvalue > data[[response]], "valore di riferimento", "valore misurato")
  min_val_lbl <- ifelse(refvalue < data[[response]], "valore di riferimento", "valore misurato")

  max_val_num <- ifelse(refvalue > data[[response]], refvalue, data[[response]])
  min_val_num <- ifelse(refvalue < data[[response]], refvalue, data[[response]])
  max_val_unc <- ifelse(refuncertainty > data[[uncertainty]], refvalue, data[[uncertainty]])
  min_val_unc <- ifelse(refuncertainty < data[[uncertainty]], refvalue, data[[uncertainty]])

  mydf <- data.frame(label = c(max_val_lbl, min_val_lbl),
                     value = c(max_val_num, min_val_num),
                     min_error = c(max_val_unc, 0),
                     max_error = c(0, min_val_unc))


  ggplot2::ggplot(data = mydf) +
    ggplot2::geom_point(
      ggplot2::aes(x = label,
                   y = value),
      size = 5,
      shape = 15,
      col = "black") +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        x = label,
        y = value,
        ymin = min_error,
        ymax = max_error
      ),
      width = 0.1
    ) +
    ggplot2::labs(x = ggplot2::element_blank(),
                  y = ylabtitle) +
    ggplot2::scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

}

#' Summary arranged on rows for two values with their extended uncertainties
#'
#' @description The function returns a table with measurement and reference values,
#'  and their extended uncertainties.
#'  Numbers are formatted as text in order to provide the desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised.
#' @param response a string with the name of the variable with the measurement values.
#' @param uncertainty a string with the name of the variable with the uncertainty values.
#' @param refvalue a \code{numeric} reference value.
#' @param refuncertainty the \code{numeric} uncertainty of the reference value.
#' @param udm a string with the unit of measurement.
#' @param signif an integer with the number of desired significant figures.
#'
#' @return a \code{data.frame} with 2 rows and 2 columns.
#'
#' @export
rowsummary_recuno <- function(data,
                              response,
                              uncertainty,
                              refvalue,
                              refuncertainty,
                              udm = "",
                              signif = 3L) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(uncertainty),
    is.numeric(refvalue),
    is.numeric(refuncertainty),
    is.character(udm),
    all.equal(signif, as.integer(signif)),
    response %in% colnames(data),
    uncertainty %in% colnames(data)
  )

  myudm <- ifelse(udm == "", "", paste0("(", udm, ")"))

  df <- data.frame(c(paste0("valore ", myudm),
                     paste0("incertezza estesa ", myudm)),
                   c(data[[response]] |> format_sigfig(), data[[uncertainty]] |> format_sigfig()),
                   c(refvalue |> format_sigfig(), refuncertainty |> format_sigfig()))
  colnames(df) <- c("statistica", "misura", "riferimento")
  df
}


#' Displays the results for the determination of trueness performances for a
#' single measurement value.
#'
#' @description The function displays the results for the determination
#' of trueness performances of an analytical method, based on a single
#' measurement value.
#' The returned text is suitable for the {SI precisa} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. At least a \code{numeric} vector with the
#'   measurements should be included.
#' @param response the name of a numeric vector in \code{data}.
#' @param significance the level of significance for the repeatability limit
#' calculation. Typical values are 0.90, 0.95 or 0.99.
#' @param refvalue a reference value for the set of measurement values.
#'
#' @details trueness expressed as recovery and bias are calculated according to
#' the Eurachem guide - The Fitness for Purpose of Analytical Methods.
#'
#' @return A list with the following items:
#'  \describe{
#'    \item{recovery}{a numeric value with the relative recovery for the
#'                    mean measurement values.}
#'    \item{bias}{a numeric value with the bias for the mean measurement values.}
#'    \item{bias_rel}{a numeric value with the relative bias for the mean measurement values.}
#'  }
#'
#' @export
#'
#' @importFrom stats sd qt
fct_trueness_recuno <- function(data,
                                response,
                                refvalue) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.numeric(refvalue),
    response %in% colnames(data)
  )

  value <- data[[response]]

  recovery <- ifelse(refvalue == 0,
                     NA,
                     ((value / refvalue) * 100))

  bias <- (value - refvalue)

  relative_bias <- (bias / value * 100)

  list(
    recovery = recovery,
    bias = bias,
    relative_bias = relative_bias
  )

}

