#' Displays the results of a \eqn{t}-test for a group of values vs a known value
#'
#' @description The function displays the results of a \eqn{t}-test performed
#'  on a group of values compared with a known value.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. A single-levels grouping \code{factor} variable
#'   and a \code{numeric} vector with the measurements should be included.
#' @param response the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param group the name of a single-level factor variable that identifies
#'   the group in \code{data}. Quotation (" ") is not required.
#' @param reflabel a character value with the name of the reference value.
#' @param reference a numeric value with the reference mean.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{t}-test is calculated using the base-R function \code{t.test}.
#'
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{mean}{a named vector of numbers, being \code{mean},
#'    \code{lwrci} and \code{uprci} the mean, lower and upper ends of the
#'    confidence interval for the provided data, respectively.
#'    The confidence interval is calculated considering both the \code{significance}
#'    and \code{alternative} arguments. For \code{alternative = "greater"} only the
#'    lower end of the confidence interval will be calculated.}
#'    \item{test}{a named vector of numbers, being \code{dof}, \code{tsper},
#'    \code{ttheo} and \code{pvalue} the degrees of freedom, the calculated value
#'    of the \eqn{t} statistic, the tabulated value of the \eqn{t} statistic and
#'    the \eqn{p}-value associated to the test. As in the original \code{t.test}
#'    function in base R, the statistic is calculated by performing a Welch test
#'    and approximating the actual number of degrees of freedom.}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
#'
#' @importFrom stats sd qt t.test
fct_ttest_1sample_mu <- function(data,
                                 response,
                                 group,
                                 reflabel,
                                 reference,
                                 significance = 0.95,
                                 alternative = "different") {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(reflabel),
    is.numeric(reference),
    response %in% colnames(data),
    group %in% colnames(data),
    alternative %in% c("different", "greater")
  )

  # defining the formula for groups
  myformula <- as.formula(paste(response, "~", group, sep = " "))
  # defining a function for a short summary
  summary_function <- function(x) c(n = length(x),
                                    mean = mean(x, na.rm = TRUE),
                                    sd = stats::sd(x, na.rm = TRUE))

  # get the summary
  mysummary <- do.call(data.frame, aggregate(myformula, data, summary_function))
  colnames(mysummary) <- c(group, "n", "mean", "sd")
  # get the group with higher mean
  max_mean <- ifelse(mysummary$mean > reference,
                     paste0("media di ", mysummary[[group]]),
                     paste0("valore di riferimento ", reflabel))
  min_mean <- ifelse(mysummary$mean <= reference,
                     paste0("media di ", mysummary[[group]]),
                     paste0("valore di riferimento ", reflabel))
  data_mean <- mysummary$mean

  # recoding the alternative hypothesis
  h1 <- switch (alternative,
                "different" = "two.sided",
                "greater" = ifelse(data_mean >= reference, "greater", "less")
  )

  # recoding the significance level based on alternative hypothesis
  alpha <- switch (alternative,
                   "different" = significance + (1 - significance)/2,
                   "greater" = significance
  )

  # # t-test results
  ttest <- stats::t.test(x = data[[response]], mu = reference,
                         alternative = h1, conf.level = significance)

  mymean <- data_mean |> format_sigfig()
  mymeanconfint <- c(NA, NA)
  mymeanconfint[1] <- ttest$conf.int[1] |> format_sigfig()
  mymeanconfint[2] <- ttest$conf.int[2] |> format_sigfig()
  tvalue <- ttest$statistic |> abs() |> (\(x) sprintf("%.4f", x))()
  dof <- ttest$parameter
  tcritical <- stats::qt(alpha, dof) |> (\(x) sprintf("%.4f", x))()
  pvalue <- ttest$p.value |> (\(x) sprintf("%.4f", x))()

  # Being clear with some text
  h0_text <- switch (alternative,
                     "different" = sprintf("%s = %s", max_mean, min_mean),
                     "greater" = sprintf("%s \u2264 %s", max_mean, min_mean),
  )

  h1_text <- switch (alternative,
                     "different" = sprintf("%s \u2260 %s", max_mean, min_mean),
                     "greater" = sprintf("%s > %s", max_mean, min_mean)
  )

  positive <- switch (alternative,
                      "different" = sprintf("%s e %s sono statisticamente differenti", max_mean, min_mean),
                      "greater" = sprintf("%s \u00E8 statisticamente maggiore di %s", max_mean, min_mean)
  )

  negative <- switch (alternative,
                      "different" = sprintf("%s e %s non sono statisticamente differenti", max_mean, min_mean),
                      "greater" = sprintf("%s non \u00E8 statisticamente maggiore di %s", max_mean, min_mean)
  )

  result <- ifelse(tvalue < tcritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       mean = c("mean" = mymean,
                "lwrci" = mymeanconfint[1],
                "uprci" = mymeanconfint[2]),
       test = c("dof" = dof |> (\(x) sprintf("%.0f", x))(),
                "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                "tsper" = unname(tvalue),
                "ttheo" = tcritical,
                "pvalue" = pvalue),
       result = unname(result))

}


#' Plotly boxplots for comparing a group of values with a reference value
#'
#' @description The function provides a simple {plotly} boxplot for comparing
#' a group of values with a reference value.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *group* with a one-level factor label for the two groups
#' to be compared, a column named *response* with the numeric values for
#' the group and a column named *outlier* with a logical vector.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param reflabel a character value with the name of the reference value.
#' @param reference a numeric value with the reference mean.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {plotly} boxplot for comparing a group of values with a reference value.
#' Raw data values are overlaid on top of the box.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers add_lines layout config
boxplot_1sample_mu <- function(data,
                               group,
                               response,
                               reflabel,
                               reference,
                               udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(reflabel),
    is.numeric(reference),
    is.character(udm),
    colnames(data) %in% c("key", "outlier", "response", "group"),
    dim(data)[2] == 4
  )

  cols <- ifelse(data$outlier == TRUE,
                 "#999999",
                 "black")

  ylabtitle <- paste0(response,
                      ifelse(udm != "", paste0(" (", udm, ")"), ""))

  hline <- function(y = 0) {
    list(
      type = "line",
      x0 = 0,
      x1 = 1,
      xref = "paper",
      y0 = y,
      y1 = y,
      line = list(width = 4,
                  dash = "dash")
    )

  }

    htext <- function(myy = 0, mylabel, myudm) {
    list(
      x = 1,
      y = myy,
      xref = "paper",
      text = paste0(mylabel, " = ", myy, " ", myudm),
      showarrow = FALSE,
      yanchor = "bottom",
      xanchor = "right"
    )

  }


  plotly::plot_ly(source = "boxplot") |>
    plotly::add_boxplot(
      data = data[data$outlier == FALSE, ],
      y = ~ response,
      x = ~ group,
      name = "boxplot",
      type = "box",
      boxmean = TRUE,
      boxpoints = FALSE,
      color = I("#2780E3"),
      showlegend = FALSE,
      key = NULL
    ) |>
    plotly::add_markers(
      data = data,
      y = ~ response,
      x = ~ group,
      name = "valori",
      marker = list(
        color = I(cols),
        colors = I(cols),
        size = 10
      ),
      key = ~ key,
      hoverinfo = "y",
      hovertemplate = paste('%{y:.3s}', udm)
    ) |>
    plotly::layout(
      showlegend = FALSE,
      title = NULL,
      xaxis = list(title = group),
      yaxis = list(title = ylabtitle,
                   hoverformat = ".3s"),
      shapes = list(hline(reference)),
      annotations = htext(reference, reflabel, udm)
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' GGplot2 boxplots for comparing a group of values with a reference value
#'
#' @description The function provides a simple {ggplot2} boxplot for comparing
#' a group of values with a reference value
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column with a two level factor label for the two groups
#' to be compared, a column with the numeric values for the two groups and a
#' column named *rimosso* with "sì" or "no" values.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param reflabel a character value with the name of the reference value.
#' @param reference a numeric value with the reference mean.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot for comparing a group of values with a reference value.
#' Raw data values are overlaid on top of the box.
#'
#' @export
#'
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_1sample_mu <- function(data,
                                 group,
                                 response,
                                 reflabel,
                                 reference,
                                 udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(reflabel),
    is.numeric(reference),
    is.character(udm)
  )

  rimosso <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")
  data$rimosso <- factor(data$rimosso, levels = c("s\u00EC", "no"))


  xlabtitle <- group
  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  quo_group <- ggplot2::ensym(group)
  quo_response <- ggplot2::ensym(response)


  ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = data[which(data$rimosso == "no"),],
                          ggplot2::aes(x = !!quo_group,
                                       y = !!quo_response),
                          fill = "white",
                          col = "black",
                          outlier.shape = NA) +
    ggplot2::geom_jitter(data = data,
                         ggplot2::aes(x = !!quo_group,
                                      y = !!quo_response,
                                      col = rimosso),
                         width = 0.2) +
    ggplot2::geom_hline(ggplot2::aes(yintercept = reference),
                        linetype='dashed') +
    ggplot2::annotate("text",
                      label = paste0(reflabel, " = ", reference, " ", udm),
                      x = 1.8,
                      y = reference,
                      vjust = -1,
                      hjust = 1) +
    ggplot2::labs(x = xlabtitle,
                  y = ylabtitle) +
    ggplot2::scale_x_discrete(expand = expansion(add = c(0.5, 0.9))) +
    ggplot2::scale_colour_manual(values = cols,
                                 breaks = c("s\u00EC", "no"),
                                 labels = c("rimosso", "non rimosso"),
                                 name = ggplot2::element_blank(),
                                 drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

}

#' Summary arranged on rows for one group vs a mean reference value
#'
#' @description The function returns a table with max, mean, median, min, sd and n
#'  values arranged on rows whereas groups are on columns. Numbers are formatted as
#'  text in order to provide the desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised.
#' @param response a string with the name of the variable to summarise.
#' @param group a string with the name of the grouping variable.
#' @param reflabel a character value with the name of the reference value.
#' @param reference a numeric value with the reference mean.
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
rowsummary_1sample_mu <- function(data,
                                  response,
                                  group,
                                  reflabel,
                                  reference,
                                  udm = "",
                                  signif = 3L) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(reflabel),
    is.numeric(reference),
    is.character(udm),
    all.equal(signif, as.integer(signif)),
    response %in% colnames(data),
    group %in% colnames(data)
  )

  statistica <- NULL
  mydata <- data.table(data)
  lvl <- mydata[[group]] |> as.factor() |> droplevels() |> levels()
  roworder <- c("n", "massimo", "media", "mediana", "minimo", "deviazione standard")
  fm <- as.formula(paste("statistica", '~', group))

  # calculate the summary
  mysummary <- mydata[, lapply(.SD, function(x) {
    c(
      n = .N,
      massimo = max(x, na.rm = TRUE) |> format_sigfig(signif),
      media = mean(x, na.rm = TRUE) |> format_sigfig(signif),
      mediana = stats::median(x, na.rm = TRUE) |> format_sigfig(signif),
      minimo = min(x, na.rm = TRUE) |> format_sigfig(signif),
      `deviazione standard` = stats::sd(x, na.rm = TRUE) |> format_sigfig(signif)
    )
  }),
  by = group,
  .SDcols = response][,
                      # table with three columns
                      "statistica" := rep(roworder, length(lvl))] |>
    data.table::dcast(eval(fm), value.var = response) |>
    # reordering rows
    (\(x) x[roworder])()

  # adding unit of measurement
  mysummary[, "statistica" := lapply(statistica, (\(x) {
    ifelse(x != "n" & udm != "", paste0(x, " (", udm, ")"), x)
  }))]

  mysummary<- cbind(mysummary, c("-", "-", reference, "-", "-", "-"))
  colnames(mysummary)[3] <- reflabel
  mysummary
}

