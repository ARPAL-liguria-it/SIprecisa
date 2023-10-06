#' Displays the results of a Shapiro-Wilk normality test
#'
#' @description The function displays the results of a Shapiro-Wilk test for
#'  normality.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param values a \code{vector} with the values relevant for testing.
#'
#' @details the Shapiro-Wilk test is calculated using the base-R function
#'  \code{shapiro.test}.
#' @return A list with the following items:
#'  \describe{
#'    \item{W}{a numeric value with the test statistic.}
#'    \item{pvalue}{a numeric value with the p-value of the test.}
#'    \item{result}{A string with the result of the test.}
#'  }
#'
#' @export
#'
#' @importFrom stats shapiro.test
fct_shapiro <- function(values) {

  stopifnot(
    is.vector(values),
    is.numeric(values)
  )

  shapiro_output <- stats::shapiro.test(values)
  result <- ifelse(shapiro_output$p.value <= 0.05,
                   "I valori non sono compatibili con una distribuzione normale",
                   "I valori sono compatibili con una distribuzione normale")


  list(W = shapiro_output$statistic[[1]],
       pvalue = shapiro_output$p.value[[1]],
       result = result)

}

#' Displays the results of a generalised extreme studentized deviate (GESD) test
#'
#' @description The function displays the results of generalised
#'  extreme studentized deviate (GESD) test for outlier detection.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param values a \code{vector} with the values relevant for testing.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param m maximum number of possible outliers to be tested.
#'   Default is one third of the number of values.
#'
#' @details the GESD test is performed according to section 4.3.2 of
#'  UNI ISO 16269-4:2019 - Statistical interpretation of data - Part 4: Detection
#'  and treatment of outliers. The software implementation in performed in
#'  accordance to Annex A of the same document.
#' @return A list with the following objects:
#' \describe{
#'    \item{data}{a dataframe with the test results.}
#'    \item{text}{a string with a summary of the test results}
#' }
#'
#' The {data} dataframe has the following columns:
#' \describe{
#'    \item{I}{Numeric values inspected by the test.}
#'    \item{R}{Numeric values for the extreme studentized deviate.}
#'    \item{lambda}{numeric values with the critical values of the test statistics.}
#'    \item{outliers}{a logical vector with the result of the test.}
#'  }
#'
#' @source UNI ISO 16269-4:2019 - Statistical interpretation of data -
#'  Part 4: Detection and treatment of outliers. Section 4.3.2 and Annex A.
#'  \url{https://store.uni.com/uni-iso-16269-4-2019}
#' @export
#'
#' @importFrom stats sd qt
fct_gesd <- function(values,
                     significance = 0.95,
                     m = round(length(values)/3, 0)) {

  stopifnot(
    is.vector(values),
    length(values) >= 5,
    is.numeric(significance),
    significance >= 0.90 & significance < 1,
    is.numeric(m),
    m <= length(values)
  )

  # function for calculating the critical lambda value
  lamba_l <- function(n_values,
                      l_removed,
                      signif) {

    alfa <- 1 - signif
    n_l <- n_values - l_removed
    p <- (1 - alfa/2)^(1/(n_l))
    tp <- stats::qt(p, n_l - 2)

    ((n_l - 1) * tp) / sqrt((n_l - 2 + tp^2) * (n_l))

  }

  # function for result as italian text in HTML
  text_result <- function(outlier_res) {

    if (sum(outlier_res$outlier) == 0) {
      "nessun valore anomalo"
    } else if (sum(outlier_res$outlier) == 1) {
      paste0(outlier_res[which(outlier_res$outlier == TRUE), "I"],
             " \u00E8 un possibile valore anomalo")
    } else {
      paste0(
        paste(outlier_res[which(outlier_res$outlier == TRUE), "I"], collapse = ", "),
        " sono possibili valori anomali")
    }

  }

  n <- length(values)
  l <- 0
  df <- data.frame(I = values)
  df_result <- data.frame()

  while (l <= m) {
    # mean and std.deviation
    x_mean <- mean(df$I)
    x_sd <- stats::sd(df$I)

    # deviates from the mean
    df$deviate <- abs(df$I - x_mean)

    # maximum studentized deviate
    df$R <- max(df$deviate)/x_sd

    # attach the maximum studentized deviate to the final results dataset
    df_result <- rbind(df_result, df[which.max(df$deviate),])

    # remove the value with the maximum deviate from the dataset
    df <- df[-which.max(df$deviate),]

    l <- l + 1
  }

 df_result$l <- 0:m
 df_result$lambda <- lamba_l(n, df_result$l, signif = significance)
 df_result$outlier <- ifelse(df_result$R > df_result$lambda, TRUE, FALSE)
 df_result <- df_result[, c("I", "R", "lambda", "outlier")]
 txt_result <- text_result(df_result)

 list(data = df_result,
      text = txt_result)

}

#' Displays the results of a \eqn{t}-test for two groups of values
#'
#' @description The function displays the results of a \eqn{t}-test performed
#'  on two groups of values.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. At least a two-levels grouping \code{factor} variable
#'   and a \code{numeric} vector with the measurements should be included.
#' @param response the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param group the name of a two-level factor variable that identifies the groups
#'   in \code{data}. Quotation (" ") is not required.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of \code{"different"} or \code{"greater"}.
#'
#' @details \eqn{t}-test is calculated using the base-R function \code{t.test},
#'  with the group with the numerically larger mean as first argument.
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
#' @importFrom stats sd qt t.test
fct_ttest_2samples <- function(data,
                      response,
                      group,
                      significance = 0.95,
                      alternative = "different") {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
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
  # get the group with higher mean
  max_mean <- mysummary[[group]][which.max(mysummary$mean)]
  min_mean <- mysummary[[group]][which.min(mysummary$mean)]
  higher_mean <- data[which(data[[group]] == max_mean),][[response]]
  lower_mean <- data[which(data[[group]] == min_mean),][[response]]
  # # t-test results
  ttest <- stats::t.test(x = higher_mean, y = lower_mean,
                         alternative = h1, conf.level = significance)
  difference <- (mean(higher_mean) - mean(lower_mean)) |> format_sigfig()
  diffconfint <- c(NA, NA)
  diffconfint[1] <- ttest$conf.int[1] |> format_sigfig()
  diffconfint[2] <- ttest$conf.int[2] |> format_sigfig()
  tvalue <- ttest$statistic |> (\(x) sprintf("%.4f", x))()
  dof <- ttest$parameter
  tcritical <- stats::qt(alpha, dof) |> (\(x) sprintf("%.4f", x))()
  pvalue <- ttest$p.value |> (\(x) sprintf("%.4f", x))()

  # Being clear with some text
  h0_text <- switch (alternative,
    "different" = sprintf("media di %s = media di %s", max_mean, min_mean),
    "greater" = sprintf("media di %s \u2264 media di %s", max_mean, min_mean),
  )

  h1_text <- switch (alternative,
    "different" = sprintf("media di %s \u2260 media di %s", max_mean, min_mean),
    "greater" = sprintf("media di %s > media di %s", max_mean, min_mean)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s sono statisticamente differenti", max_mean, min_mean),
                      "greater" = sprintf("la media di %s \u00E8 statisticamente maggiore della media di %s", max_mean, min_mean)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la media di %s e la media di %s non sono statisticamente differenti", max_mean, min_mean),
                      "greater" = sprintf("la media di %s non \u00E8 statisticamente maggiore della media di %s", max_mean, min_mean)
  )

  result <- ifelse(tvalue < tcritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       difference = c("mean" = difference,
                      "lwrci" = diffconfint[1],
                      "uprci" = diffconfint[2]),
       test = c("dof" = dof |> (\(x) sprintf("%.4f", x))(),
                "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                "tsper" = unname(tvalue),
                "ttheo" = tcritical,
                "pvalue" = pvalue),
       result = result)

}

#' Displays the results of an \eqn{F}-test for two groups of values
#'
#' @description The function displays the results of an \eqn{F}-test performed
#'  on two groups of values.
#'  The returned text is suitable for the {SI confronta} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. At least a two-levels grouping \code{factor} variable
#'   and a \code{numeric} vector with the measurements should be included.
#' @param response the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param group the name of a two-level factor variable that identifies the groups
#'   in \code{data}. Quotation (" ") is not required.
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
fct_ftest_2samples <- function(data,
                      response,
                      group,
                      significance = 0.95,
                      alternative = "different") {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
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
  mysummary <- do.call(data.frame, stats::aggregate(myformula, data, summary_function))
  colnames(mysummary) <- c(group, "n", "mean", "sd")
  # get the group with higher sd
  max_sd <- mysummary[[group]][which.max(mysummary$sd)]
  min_sd <- mysummary[[group]][which.min(mysummary$sd)]
  higher_sd <- data[which(data[[group]] == max_sd),][[response]]
  lower_sd <- data[which(data[[group]] == min_sd),][[response]]
  # # F-test results
  ftest <- stats::var.test(x = higher_sd, y = lower_sd,
                  alternative = h1, conf.level = significance)
  ratio <- (stats::sd(higher_sd)^2 / stats::sd(lower_sd)^2) |> format_sigfig()
  ratioconfint <- c(NA, NA)
  ratioconfint[1] <- ftest$conf.int[1] |> format_sigfig()
  ratioconfint[2] <- ftest$conf.int[2] |> format_sigfig()
  fvalue <- ftest$statistic |> (\(x) sprintf("%.4f", x))()
  dof <- ftest$parameter |> unname() # numerator and denominator
  fcritical <- c(stats::qf(1-alpha, dof[[1]], dof[[2]]),
                 stats::qf(alpha, dof[[1]], dof[[2]])) |> round(5)
  pvalue <- ftest$p.value |> (\(x) sprintf("%.4f", x))()

  # Being clear with some text
  h0_text <- switch (alternative,
                     "different" = sprintf("varianza di %s = varianza di %s", max_sd, min_sd),
                     "greater" = sprintf("varianza di %s \u2264 varianza di %s", max_sd, min_sd),
  )

  h1_text <- switch (alternative,
                     "different" = sprintf("varianza di %s \u2260 varianza di %s", max_sd, min_sd),
                     "greater" = sprintf("varianza di %s > varianza di %s", max_sd, min_sd)
  )

  positive <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di %s sono statisticamente differenti", max_sd, min_sd),
                      "greater" = sprintf("la varianza di %s \u00E8 statisticamente maggiore della varianza di %s", max_sd, min_sd)
  )

  negative <- switch (alternative,
                      "different" = sprintf("la varianza di %s e la varianza di %s non sono statisticamente differenti", max_sd, min_sd),
                      "greater" = sprintf("la varianza di %s non \u00E8 statisticamente maggiore della varianza di %s", max_sd, min_sd)
  )

  result <- switch (alternative,
                    "different" = ifelse(fvalue > fcritical[1] & fvalue < fcritical[2],
                                         negative,
                                         positive),
                    "greater" = ifelse(fvalue < fcritical[2],
                                       negative,
                                       positive)
  )

  ftheo <- switch (alternative,
                    "different" = paste0(fcritical[1] |> (\(x) sprintf("%.4f", x))(),
                                         ", ",
                                         fcritical[2] |> (\(x) sprintf("%.4f", x))()),
                    "greater" = paste0(fcritical[2] |> (\(x) sprintf("%.4f", x))())
  )



  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       ratio = c("mean" = ratio,
                      "lwrci" = ratioconfint[1],
                      "uprci" = ratioconfint[2]),
       test = list("dof" = c("numeratore" = dof[[1]] |> (\(x) sprintf("%.0f", x))(),
                             "denominatore" = dof[[2]] |> (\(x) sprintf("%.0f", x))()),
                "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                "fsper" = unname(fvalue),
                "ftheo" = ftheo,
                "pvalue" = pvalue),
       result = unname(result))

}

#' Plotly boxplots for comparing two groups of values
#'
#' @description The function provides a simple {plotly} boxplot for comparing
#' two groups of values
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *group* with a two level factor label for the two groups
#' to be compared, a column named *response* with the numeric values for
#' the two groups and a column named *outlier* with a logical vector.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {plotly} boxplot for comparing two group of values. Raw data values
#' are overlaid on top of the boxes.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers layout config
boxplot_2samples <- function(data,
                             group,
                             response,
                             udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(udm),
    colnames(data) %in% c("key", "outlier", "response", "group"),
    dim(data)[2] == 4
  )

  cols <- ifelse(data$outlier == TRUE,
                          "#999999",
                          "black")

  ylabtitle <- paste0(response,
           ifelse(udm != "", paste0(" (", udm, ")"), ""))


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
                   hoverformat = ".3s")
    ) |>
    plotly::config(displayModeBar = FALSE,
                   locale = "it")

}

#' GGplot2 boxplots for comparing two groups of values
#'
#' @description The function provides a simple {ggplot2} boxplot for comparing
#' two groups of values
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column with a two level factor label for the two groups
#' to be compared, a column with the numeric values for the two groups and a
#' column named *rimosso* with "sì" or "no" values.
#' @param group a character string for the label of the grouping variable.
#' @param response a character string with the label for the response numeric variable.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot for comparing two group of values. Raw data values
#' are overlaid on top of the boxes.
#'
#' @export
#'
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_2samples <- function(data,
                             group,
                             response,
                             udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
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
    ggplot2::labs(x = xlabtitle,
                  y = ylabtitle) +
    ggplot2::scale_color_manual(values = cols,
                                breaks = c("s\u00EC", "no"),
                                labels = c("rimosso", "non rimosso"),
                                name = ggplot2::element_blank(),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

}

#' Summary arranged on rows for two groups
#'
#' @description The function returns a table with max, mean, median, min, sd and n
#'  values arranged on rows whereas groups are on columns. Numbers are formatted as
#'  text in order to provide the desired significant figures.
#'
#' @param data the \code{data.frame} or \code{data.table} to be summarised.
#' @param response a string with the name of the variable to summarise.
#' @param group a string with the name of the grouping variable.
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
rowsummary_2samples <- function(data,
                                response,
                                group,
                                udm = "",
                                signif = 3L) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.character(group),
    is.character(udm),
    all.equal(signif, as.integer(signif)),
    response %in% colnames(data),
    group %in% colnames(data)
  )

  statistica <- NULL
  mydata <- data.table(data)
  lvl <- levels(as.factor(mydata[[group]]))
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
}

