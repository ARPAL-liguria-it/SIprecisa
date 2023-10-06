#' Displays the results of a \eqn{\chi^2}-test for a group of values vs a
#' reference standard deviation value.
#'
#' @description The function displays the results of a \eqn{\chi^2}-test performed
#'  on a group of values vs a reference standard deviation value.
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
#' @param reference a numeric value with the reference standard deviation.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{\chi^2}-test is with the numerically larger variance as numerator.
#'  As a consequence, the alternative  hypothesis tested can only be
#'  \eqn{\mathrm{Var}(A) \neq \mathrm{Var}(B)}  (\code{alternative = "different"})
#'  or \eqn{\mathrm{Var}(A) > \mathrm{Var}(B)} (\code{alternative = "greater"}).
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{ratio}{a named vector of numbers, being \code{sd},
#'    \code{lwrci} and \code{uprci} the variance of the provided numeric values and
#'    the lower and upper ends of the confidence interval, respectively.
#'    The confidence interval is calculated considering both the \code{significance}
#'    and \code{alternative} arguments. For \code{alternative = "greater"} only the
#'    lower end of the confidence interval will be calculated.}
#'    \item{test}{a named vector of numbers, being \code{dof}, \code{chisper},
#'    \code{chitheo} and \code{pvalue} the degrees of freedom, the calculated value
#'    of the \eqn{\chi^2} statistic, the tabulated value of the \eqn{\chi^2} statistic and
#'    the \eqn{p}-value associated to the test.}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
#'
#' @importFrom stats aggregate sd qchisq
fct_chitest_1sample_sigma <- function(data,
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

  # recoding the alternative hypothesis
  h1 <- switch (alternative,
                "different" = "two.sided",
                "greater" = "greater"
  )

  # recoding the significance level based on alternative hypothesis
  alpha <- switch (alternative,
                   "different" = significance + (1 - significance)/2,
                   "greater" = significance
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

  #get the sample label
  data_lbl <- mysummary[[group]]
  # get the sample mean
  data_sd <- mysummary$sd
  # get degree of freedoms
  dof <- c(mysummary$n -1)


    # get chi-squared statitistics
  chivalue <- (data_sd^2 * dof) / reference^2


  # get the critical chi-squared value
  chicritical <- if(h1 == "two.sided")
    c(stats::qchisq(1-alpha, dof), stats::qchisq(alpha, dof))
  else
    stats::qchisq(alpha, dof)

  # get the confidence interval
  ci <- sqrt( (dof * data_sd^2) / chicritical)
  ci <- ci[order(ci)]
  ci <- if (h1 == "two.sided") {
    ci
  } else {
    c(ci, Inf) }

  # p value
  side <- ifelse(data_sd > reference, FALSE, TRUE)
  pvalue <- ifelse(h1 == "two.sided",
                   stats::pchisq(chivalue, dof, lower.tail = side)*2,
                   stats::pchisq(chivalue, dof, lower.tail = FALSE))

  # Being clear with some text
  h0_text <- switch (alternative,
                     "different" = sprintf("varianza di %s = varianza di riferimento %s", data_lbl, reflabel),
                     "greater" = sprintf("varianza di %s \u2264 varianza di riferimento %s", data_lbl, reflabel)
  )

  h1_text <- switch (alternative,
                     "different" = sprintf("varianza di %s \u2260 varianza di riferimento %s", data_lbl, reflabel),
                     "greater" = sprintf("varianza di %s > varianza di riferimento %s", data_lbl, reflabel)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di riferimento %s sono statisticamente differenti", data_lbl, reflabel),
                      "greater" = sprintf("la varianza di %s \u00E8 statisticamente maggiore della varianza di riferimento %s", data_lbl, reflabel)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di riferimento %s non sono statisticamente differenti", data_lbl, reflabel),
                      "greater" = sprintf("la varianza di %s non \u00E8 statisticamente maggiore della varianza di riferimento %s", data_lbl, reflabel)
  )

  result <- switch (alternative,
                    "different" = ifelse(chivalue > chicritical[1] & chivalue < chicritical[2],
                                         negative,
                                         positive),
                    "greater" = ifelse(chivalue < chicritical,
                                       negative,
                                       positive)
  )

  chitheo <- switch (alternative,
                     "different" = paste0(chicritical[1] |> (\(x) sprintf("%.4f", x))(), ", ",
                                        chicritical[2] |> (\(x) sprintf("%.4f", x))()),
                     "greater" = paste0(chicritical |> (\(x) sprintf("%.4f", x))())
  )



  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       ratio = c("sd" = data_sd |> format_sigfig(),
                 "lwrci" = ci[1] |> format_sigfig(),
                 "uprci" = ci[2] |> format_sigfig()),
       test = list("dof" = dof |> (\(x) sprintf("%.0f", x))(),
                   "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                   "chisper" = chivalue |> (\(x) sprintf("%.4f", x))(),
                   "chitheo" = chitheo,
                   "pvalue" = pvalue |> (\(x) sprintf("%.4f", x))()),
       result = result)

}


#' Plotly boxplots for comparing a group of values with a reference standard
#' deviation value
#'
#' @description The function provides a simple {plotly} boxplot for comparing
#' a group of values with a reference standard deviation value.
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
#' @return A {plotly} boxplot for comparing a group of values with a reference
#' standard deviation value.
#' Raw data values are overlaid on top of the box.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers add_lines layout config
boxplot_1sample_sigma <- function(data,
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

  data_mean <- data$response |> mean()

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

    htext <- function(mean = 0, myy = 0, mylabel, myudm, lbl, myanchor) {
      stopifnot(myanchor %in% c("top", "bottom"))

    list(
      x = 1,
      y = mean,
      xref = "paper",
      text = paste0(mylabel, " = ", lbl, myy, " ", myudm),
      showarrow = FALSE,
      yanchor = myanchor,
      xanchor = "right"
    )

    }

    lowlbl <- paste0("media - 2 \u00D7 ")
    uplbl <- paste0("media + 2 \u00D7 ")

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
      shapes = list(hline(data_mean + 2 * reference),
                    hline(data_mean - 2 * reference)),
      annotations = list(htext(data_mean - 2 * reference, reference, reflabel,
                               udm, lowlbl, "bottom"),
                         htext(data_mean + 2 * reference, reference, reflabel,
                               udm, uplbl, "top"))
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' GGplot2 boxplots for comparing a group of values with a standard deviation
#' reference value
#'
#' @description The function provides a simple {ggplot2} boxplot for comparing
#' a group of values with a standard deviation reference value
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
ggboxplot_1sample_sigma <- function(data,
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

  data_mean <- data[which(data$rimosso == "no")][[response]] |> mean()

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
    ggplot2::geom_hline(ggplot2::aes(yintercept = data_mean + c(-2, 2) *reference),
                        linetype = 'dashed') +
    ggplot2::annotate("text",
                      label = paste0(reflabel, " = ", "media", c(" - 2 \u00D7 ", " + 2 \u00D7 "), reference, " ", udm),
                      x = 1.8,
                      y = data_mean + c(-2, 2) * reference,
                      vjust = c(-1, +1),
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

#' Summary arranged on rows for one group vs a standard deviation reference value
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
rowsummary_1sample_sigma <- function(data,
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

  mysummary<- cbind(mysummary, c("-", "-", "-", "-", "-", reference))
  colnames(mysummary)[3] <- reflabel
  mysummary
}

