#' Displays the results of a \eqn{t}-test for two groups of values for one of
#' which only mean, standard deviation and number of samples are reported.
#'
#' @description The function displays the results of a \eqn{t}-test performed
#'  on two groups of values, for one of which only mean, standard deviation and
#'  number of samples are reported.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param group1 a character value with the name of the first group.
#' @param mean1 a numeric value with the mean for the first group.
#' @param sd1 a numeric value with the standard deviation for the first group.
#' @param n1 a numeric value with the number of values for the first group.
#' @param group2 a character value with the name of the second group.
#' @param mean2 a numeric value with the mean for the second group.
#' @param sd2 a numeric value with the standard deviation for the second group.
#' @param n2 a numeric value with the number of values for the second group.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{t}-test is calculated using the group with the numerically
#'  larger mean as first argument.
#'  As a consequence, for the means of two series of values \eqn{A} and \eqn{B},
#'  being the first one numerically greater than the second one, the alternative
#'  hypothesis tested can only be \eqn{\bar{A} \neq \bar{B}}
#'  (\code{alternative = "different"}) or \eqn{\bar{A} > \bar{B}}
#'  (\code{alternative = "greater"}).
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{difference}{a named vector of numbers, being \code{mean},
#'    \code{lwrci} and \code{uprci} the difference in means of the two groups and
#'    the lower and upper ends of the confidence interval, respectively.
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
#' @importFrom stats sd qt pt
fct_ttest_2samples_par <- function(group1,
                                   mean1,
                                   sd1,
                                   n1,
                                   group2,
                                   mean2,
                                   sd2,
                                   n2,
                                   significance = 0.95,
                                   alternative = "different") {
  stopifnot(
    is.numeric(mean1),
    is.numeric(mean2),
    is.numeric(sd1),
    is.numeric(sd2),
    is.numeric(n1),
    is.numeric(n2),
    is.character(group1),
    is.character(group2),
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


  mysummary <- data.frame(group = c(group1, group2),
                          n = c(n1, n2),
                          mean = c(mean1, mean2),
                          sd = c(sd1, sd2))
  colnames(mysummary) <- c("group", "n", "mean", "sd")

  # get the group with higher mean
  max_mean_gr <- mysummary$group[which.max(mysummary$mean)]
  min_mean_gr <- mysummary$group[which.min(mysummary$mean)]

  # get the mean, sd and n associated with the max and min means values
  max_mean_vl <- max(mysummary$mean)
  min_mean_vl <- min(mysummary$mean)
  max_mean_sd <- mysummary$sd[which.max(mysummary$mean)]
  min_mean_sd <- mysummary$sd[which.min(mysummary$mean)]
  max_mean_n <- mysummary$n[which.max(mysummary$mean)]
  min_mean_n <- mysummary$n[which.min(mysummary$mean)]

  # get the standard errors for the group with max and min means
  max_mean_stderr <- max_mean_sd/sqrt(max_mean_n)
  min_mean_stderr <- min_mean_sd/sqrt(min_mean_n)

  # t-test numerator
  diff_mean <- max_mean_vl - min_mean_vl
  # t-test denominator
  diff_stderr <- sqrt(max_mean_stderr^2 + min_mean_stderr^2)
  # t value
  tvalue <- diff_mean/diff_stderr

  # degree of freedom
  dof <- (max_mean_sd^2/max_mean_n + min_mean_sd^2/min_mean_n)^2/
    ( (max_mean_sd^4/(max_mean_n^2 * (max_mean_n - 1) )) +
        (min_mean_sd^4/(min_mean_n^2 * (min_mean_n - 1))) )

  # t critical and p-value
  tcritical <- stats::qt(alpha, dof)
  pvalue <- ifelse(h1 == "two.sided",
                   2 * stats::pt(tvalue, dof, lower.tail = FALSE),
                   stats::pt(tvalue, dof, lower.tail = FALSE))

  # difference confidence interval
  ci <- diff_mean + c(-1, 1) * tcritical * diff_stderr

  # Being clear with some text
  h0_text <- switch (alternative,
    "different" = sprintf("media di %s = media di %s", max_mean_gr, min_mean_gr),
    "greater" = sprintf("media di %s \u2264 media di %s", max_mean_gr, min_mean_gr),
  )

  h1_text <- switch (alternative,
    "different" = sprintf("media di %s \u2260 media di %s", max_mean_gr, min_mean_gr),
    "greater" = sprintf("media di %s > media di %s", max_mean_gr, min_mean_gr)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s sono statisticamente differenti",
                                            max_mean_gr, min_mean_gr),
                      "greater" = sprintf("la media di %s \u00E8 statisticamente maggiore della media di %s",
                                          max_mean_gr, min_mean_gr)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s non sono statisticamente differenti",
                                            max_mean_gr, min_mean_gr),
                      "greater" = sprintf("la media di %s non \u00E8 statisticamente maggiore della media di %s",
                                          max_mean_gr, min_mean_gr)
  )

  result <- ifelse(tvalue < tcritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       difference = c("mean" = diff_mean |> format_sigfig(),
                      "lwrci" = ci[1] |> format_sigfig(),
                      "uprci" = ifelse(h1 == "two.sided", ci[2] |> format_sigfig(), Inf)),
       test = c("dof" = dof |> (\(x) sprintf("%.4f", x))(),
                "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                "tsper" = tvalue |> (\(x) sprintf("%.4f", x))(),
                "ttheo" = tcritical |> (\(x) sprintf("%.4f", x))(),
                "pvalue" = pvalue |> (\(x) sprintf("%.4f", x))()),
       result = result)

}

#' Displays the results of an \eqn{F}-test for two groups of values for one of
#' which only mean, standard deviation and number of samples are reported.
#'
#' @description The function displays the results of an \eqn{F}-test performed
#'  on two groups of values, for one of which only mean, standard deviation and
#'  number of samples are reported.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param group1 a character value with the name of the first group.
#' @param sd1 a numeric value with the standard deviation for the first group.
#' @param n1 a numeric value with the number of values for the first group.
#' @param group2 a character value with the name of the second group.
#' @param sd2 a numeric value with the standard deviation for the second group.
#' @param n2 a numeric value with the number of values for the second group.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{F}-test is calculated using the base-R function \code{var.test},
#'  with the group with the numerically larger variance as first argument.
#'  As a consequence, for the variances of two series of values \eqn{A} and \eqn{B},
#'  being the first one numerically greater than the second one, the alternative
#'  hypothesis tested can only be \eqn{\mathrm{Var}(A) \neq \mathrm{Var}(B)}
#'  (\code{alternative = "different"}) or \eqn{\mathrm{Var}(A) > \mathrm{Var}(B)}
#'  (\code{alternative = "greater"}).
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{ratio}{a named vector of numbers, being \code{mean},
#'    \code{lwrci} and \code{uprci} the ratio of the variances of the two groups and
#'    the lower and upper ends of the confidence interval, respectively.
#'    The confidence interval is calculated considering both the \code{significance}
#'    and \code{alternative} arguments. For \code{alternative = "greater"} only the
#'    lower end of the confidence interval will be calculated.}
#'    \item{test}{a named vector of numbers, being \code{dof}, \code{fsper},
#'    \code{ftheo} and \code{pvalue} the degrees of freedom, the calculated value
#'    of the \eqn{F} statistic, the tabulated value of the \eqn{F} statistic and
#'    the \eqn{p}-value associated to the test.}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
#'
#' @importFrom stats aggregate var.test sd qf
fct_ftest_2samples_par <- function(group1,
                                   sd1,
                                   n1,
                                   group2,
                                   sd2,
                                   n2,
                                   significance = 0.95,
                                   alternative = "different") {
  stopifnot(
    is.character(group1),
    is.character(group2),
    is.numeric(sd1),
    is.numeric(sd2),
    is.numeric(n1),
    is.numeric(n2),
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

  mysummary <- data.frame(group = c(group1, group2),
                          n = c(n1, n2),
                          sd = c(sd1, sd2))
  colnames(mysummary) <- c("group", "n", "sd")

  # get the group with higher sd
  max_sd_gr <- mysummary$group[which.max(mysummary$sd)]
  min_sd_gr <- mysummary$group[which.min(mysummary$sd)]

  # get sd and n associated with the max and min sd values
  max_sd_val <- mysummary$sd[which.max(mysummary$sd)]
  min_sd_val <- mysummary$sd[which.min(mysummary$sd)]
  max_sd_n <- mysummary$n[which.max(mysummary$sd)]
  min_sd_n <- mysummary$n[which.min(mysummary$sd)]

  # get F statitistics
  fvalue <- max_sd_val^2 / min_sd_val^2

  # get degree of freedoms
  dof <- c(max_sd_n - 1, min_sd_n - 1)

  # get the critical F value
  fcritical <- if(h1 == "two.sided")
    c(stats::qf(1-alpha, dof[1], dof[2]), stats::qf(alpha, dof[1], dof[2]))
  else
    c(stats::qf(alpha, dof[1], dof[2]))

  # get the confidence interval
  ci <- fvalue / fcritical
  ci <- ci[order(ci)]
  ci[2] <- ifelse(h1 == "two.sided",
                  ci[2],
                  Inf)
  # p value
  pvalue <- ifelse(h1 == "two.sided",
                   stats::pf(fvalue, dof[1], dof[2], lower.tail = FALSE) + stats::pf(1/fvalue, dof[2], dof[1], lower.tail = TRUE),
                   stats::pf(fvalue, dof[1], dof[2], lower.tail = FALSE))

  # Being clear with some text
  h0_text <- switch (alternative,
                     "different" = sprintf("varianza di %s = varianza di %s", max_sd_gr, min_sd_gr),
                     "greater" = sprintf("varianza di %s \u2264 varianza di %s", max_sd_gr, min_sd_gr),
  )

  h1_text <- switch (alternative,
                     "different" = sprintf("varianza di %s \u2260 varianza di %s", max_sd_gr, min_sd_gr),
                     "greater" = sprintf("varianza di %s > varianza di %s", max_sd_gr, min_sd_gr)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di %s sono statisticamente differenti", max_sd_gr, min_sd_gr),
                      "greater" = sprintf("la varianza di %s \u00E8 statisticamente maggiore della varianza di %s", max_sd_gr, min_sd_gr)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di %s non sono statisticamente differenti", max_sd_gr, min_sd_gr),
                      "greater" = sprintf("la varianza di %s non \u00E8 statisticamente maggiore della varianza di %s", max_sd_gr, min_sd_gr)
  )

  result <- switch (alternative,
                    "different" = ifelse(fvalue > fcritical[1] & fvalue < fcritical[2],
                                         negative,
                                         positive),
                    "greater" = ifelse(fvalue < fcritical,
                                       negative,
                                       positive)
  )

  ftheo <- switch (alternative,
                    "different" = paste0(fcritical[1] |> (\(x) sprintf("%.4f", x))(), ", ",
                                         fcritical[2] |> (\(x) sprintf("%.4f", x))()),
                    "greater" = paste0(fcritical |> (\(x) sprintf("%.4f", x))())
  )



  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       ratio = c("mean" = fvalue |> format_sigfig(),
                      "lwrci" = ci[1] |> format_sigfig(),
                      "uprci" = ci[2] |> format_sigfig()),
       test = list("dof" = c("numeratore" = dof[1] |> (\(x) sprintf("%.0f", x))(),
                             "denominatore" = dof[2] |> (\(x) sprintf("%.0f", x))()),
                "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                "fsper" = fvalue |> (\(x) sprintf("%.4f", x))(),
                "ftheo" = ftheo,
                "pvalue" = pvalue |> (\(x) sprintf("%.4f", x))()),
       result = result)

}

#' Plotly boxplots for comparing two groups of values for one of which only mean,
#' standard deviation and number of samples are reported.
#'
#' @description The function provides a simple {plotly} boxplot for comparing
#' two groups of values for one of which only mean, standard deviation and number
#' of samples are reported.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *group* with a one level factor label, a column named *response*
#' with the numeric values and a column named *outlier* with a logical vector.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#' @param group2 a character string for the name of the group for which only the
#' summary has been provided.
#' @param dfsummary a data.frame with mean, sd and number of values for the two
#' groups: it has three rows and three columns. Column names are "index",
#' name of the group1 and name of the group2.
#'
#' @return A {plotly} boxplot for comparing two group of values. Raw data values
#' are overlaid on top of the boxes.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers layout config
boxplot_2samples_par <- function(data,
                                 group,
                                 response,
                                 udm,
                                 group2,
                                 dfsummary) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(dfsummary),
    is.character(response),
    is.character(group),
    is.character(group2),
    is.character(udm),
    colnames(data) %in% c("key", "outlier", "response", "group"),
    c("index", group2) %in% colnames(dfsummary),
    dfsummary$index %in% c("mean", "sd", "n"),
    dim(data)[2] == 4,
    dim(dfsummary) == c(3, 3)
  )

  quantile <- NULL

  group1 <- data$group |> droplevels() |> levels()
  # quantiles and fence limits for group 1 (it is the group with all the values)
  quant_gr1 <- quantile(data[data$outlier == FALSE, "response"], probs = c(0.25, 0.50, 0.75))
  fence_gr1 <- c(quant_gr1[1] - (quant_gr1[3] - quant_gr1[1])*1.5, quant_gr1[3] + (quant_gr1[3] - quant_gr1[1])*1.5)

  # quantiles and fence limits for group 2 (it is the group with summarised values)
  mean_gr2 <- dfsummary[[group2]][which(dfsummary$index == "mean")]
  sd_gr2 <- dfsummary[[group2]][which(dfsummary$index == "sd")]
  quant_gr2 <- mean_gr2 + c(-1, 0, 1) * sd_gr2
  fence_gr2 <- c(quant_gr2[1] - (quant_gr2[3] - quant_gr2[1])*1.5, quant_gr2[3] + (quant_gr2[3] - quant_gr2[1])*1.5)

  # gathering all the box limits into a data.frame
  mybox <- data.frame(group_fct = factor(c(group1, group2)),
                      lowerfence = c(fence_gr1[1], fence_gr2[1]),
                      q1 = c(quant_gr1[1], quant_gr2[1]),
                      mean = c(mean(data[data$outlier == FALSE, "response"]), mean_gr2),
                      median = c(quant_gr1[2], quant_gr2[2]),
                      q3 = c(quant_gr1[3], quant_gr2[3]),
                      upperfence = c(fence_gr1[2], fence_gr2[2]))


  quo_group <- enquote(group)
  quo_response <- enquote(response)
  quo_udm <- enquote(udm)

  cols <- ifelse(data$outlier == TRUE,
                          "#999999",
                          "black")

  ylabtitle <- paste0(response,
           ifelse(udm != "", paste0(" (", udm, ")"), ""))


  plotly::plot_ly(source = "boxplot") |>
    plotly::add_boxplot(
      data = mybox,
      x = ~ group_fct,
      lowerfence = ~ lowerfence,
      q1 = ~ q1,
      median = ~ median,
      mean = ~ mean,
      q3 = ~ q3,
      upperfence = ~ upperfence,
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
                   hoverformat = ".3s")
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' GGplot2 boxplots for comparing two groups of values, for one of which only mean,
#' standard deviation and number of samples are reported.
#'
#' @description The function provides a simple {ggplot2} boxplot for comparing
#' two groups of values for one of which only mean, standard deviation and number
#' of samples are reported.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column with a two level factor label for the two groups
#' to be compared, a column with the numeric values for the two groups and a
#' column named *rimosso* with "sì" or "no" values.
#' @param group a character string for the label of the grouping variable.
#' @param group2 a character string for the name of the group for which only the
#' summary has been provided.
#' @param dfsummary a data.frame with mean, sd and number of values for the two
#' groups: it has three rows and three columns. Column names are "index",
#' name of the group1 and name of the group2.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot for comparing two group of values. Raw data values
#' are overlaid on top of the boxes.
#'
#' @export
#'
#' @import data.table
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_2samples_par <- function(data,
                                   group,
                                   response,
                                   udm,
                                   group2,
                                   dfsummary) {
  stopifnot(
    is.data.frame(data),
    is.data.frame(dfsummary),
    is.character(response),
    is.character(group),
    is.character(group2),
    is.character(udm),
    c("index", group2) %in% colnames(dfsummary),
    dfsummary$index %in% c("mean", "sd", "n"),
    dim(dfsummary) == c(3, 3)
  )

  quantile <- NULL
  group_fct <- NULL
  lowerfence <- NULL
  q1 <- NULL
  q2 <- NULL
  q3 <- NULL
  upperfence <- NULL
  dt <- data.table::data.table(data)
  # getting the label for the first group
  group1 <- dt[[group]] |> droplevels() |> levels()

  # getting quantiles and fence limits for the first group
  quant_gr1 <- dt[which(dt$rimosso == "no")][[response]] |>
    quantile(probs = c(0.25, 0.50, 0.75))
  fence_gr1 <- c(
      quant_gr1[1] - (quant_gr1[3] - quant_gr1[1]) * 1.5,
      quant_gr1[3] + (quant_gr1[3] - quant_gr1[1]) * 1.5
      )
  # getting quantiles and fence limits for the first group
  mean_gr2 <- dfsummary[[group2]][which(dfsummary$index == "mean")]
  sd_gr2 <- dfsummary[[group2]][which(dfsummary$index == "sd")]
  quant_gr2 <- mean_gr2 + c(-1, 0, 1) * sd_gr2
  fence_gr2 <- c(
      quant_gr2[1] - (quant_gr2[3] - quant_gr2[1]) * 1.5,
      quant_gr2[3] + (quant_gr2[3] - quant_gr2[1]) * 1.5
      )

  # building the summary dataframe
  mybox <- data.frame(
    group_fct = factor(c(group1, group2)),
    lowerfence = c(fence_gr1[1], fence_gr2[1]),
    q1 = c(quant_gr1[1], quant_gr2[1]),
    mean = c(mean(dt[which(dt$rimosso == "no")][[response]]), mean_gr2),
    median = c(quant_gr1[2], quant_gr2[2]),
    q3 = c(quant_gr1[3], quant_gr2[3]),
    upperfence = c(fence_gr1[2], fence_gr2[2])
  )
  rownames(mybox) <- NULL

  # colours and axis labels
  rimosso <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")
  data$rimosso <- factor(data$rimosso, levels = c("s\u00EC", "no"))
  xlabtitle <- group
  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  quo_group <- ggplot2::ensym(group)
  quo_response <- ggplot2::ensym(response)


  ggplot2::ggplot() +
    ggplot2::geom_boxplot(
      data = mybox,
      ggplot2::aes(
        x = group_fct,
        ymin = lowerfence,
        lower = q1,
        middle = median,
        upper = q3,
        ymax = upperfence
      ),
      fill = "white",
      col = "black",
      stat = "identity",
      outlier.shape = NA
    ) +
    ggplot2::geom_jitter(
      data = dt,
      ggplot2::aes(
        x = !!quo_group,
        y = !!quo_response,
        col = rimosso
      ),
      width = 0.2
    ) +
    ggplot2::labs(x = xlabtitle,
                  y = ylabtitle) +
    ggplot2::scale_color_manual(
      values = cols,
      breaks = c("s\u00EC", "no"),
      labels = c("rimosso", "non rimosso"),
      name = ggplot2::element_blank(),
      drop = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

}

#' Summary arranged on rows for two groups for one of which only mean, standard deviation
#' and number of samples are reported.
#'
#' @description The function returns a table with max, mean, median, min, sd and n
#'  values arranged on rows whereas groups are on columns. Numbers are formatted as
#'  text in order to provide the desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised.
#' @param response a string with the name of the variable to summarise.
#' @param group a string with the name of the grouping variable.
#' @param group2 a character value with the name of the second group.
#' @param mean2 a numeric value with the mean for the second group.
#' @param sd2 a numeric value with the standard deviation for the second group.
#' @param n2 a numeric value with the number of values for the second group.
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
rowsummary_2samples_par <- function(data,
                                    response,
                                    group,
                                    group2,
                                    mean2,
                                    sd2,
                                    n2,
                                    udm = "",
                                    signif = 3L) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(group2),
    is.numeric(mean2),
    is.numeric(sd2),
    is.numeric(n2),
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

    mysummary<- cbind(mysummary, c(n2, "-", mean2, "-", "-", sd2))
    colnames(mysummary)[3] <- group2
    mysummary
}

