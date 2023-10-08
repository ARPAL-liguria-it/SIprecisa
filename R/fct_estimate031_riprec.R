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

#' Displays the results of a \eqn{t}-test for a group of values vs a reference value.
#'
#' @description The function displays the results of a \eqn{t}-test performed
#'  on a group of values vs a reference value.
#'  The returned text is suitable for the {SI precisa} {shiny} app.
#'
#' @param data a \code{data.frame} or \code{data.table} with the results
#'   relevant for testing. At least a \code{numeric} vector with the
#'   measurements should be included.
#' @param response the name of a numeric vector in \code{data}.
#'   Quotation (" ") is not required.
#' @param significance a number, typically either 0.90, 0.95 (default) or 0.99
#'   indicating the confidence level for the test.
#'
#' @details \eqn{t}-test is calculated using the base-R function \code{t.test}
#'  with a two-sided alternative hypothesis.
#'
#' @return A list with the following items:
#'  \describe{
#'    \item{hypotheses}{a named vector of strings, being \code{h0} and \code{h1}
#'    the null and alternative hypothesis, respectively.}
#'    \item{difference}{a named vector of numbers, being \code{mean},
#'    \code{lwrci} and \code{uprci} the mean of the measurement value, and
#'    the lower and upper end of its confidence interval, respectively.}
#'    \item{test}{a named vector of numbers, being \code{dof}, \code{tsper},
#'    \code{ttheo} and \code{pvalue} the degrees of freedom, the calculated value
#'    of the \eqn{t} statistic, the tabulated value of the \eqn{t} statistic and
#'    the \eqn{p}-value associated to the test. As in the original \code{t.test}
#'    function in base R, the statistic is calculated by performing a Student t-test}
#'    \item{result}{a string indicating whether H0 should be rejected or not.}
#'  }
#'
#' @export
#'
#' @importFrom stats sd qt t.test
fct_ttest_riprec <- function(data,
                             response,
                             refvalue,
                             significance = 0.95) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.numeric(refvalue),
    response %in% colnames(data)
  )

  # recoding the significance level
  alpha <- significance + (1 - significance)/2

  data_mean <- mean(data[[response]], na.rm = TRUE)

  max_mean <- ifelse(data_mean > refvalue,
                     "media delle misure",
                     "valore di riferimento")
  min_mean <- ifelse(data_mean <= refvalue,
                     "media delle misure",
                     "valore di riferimento")

  # # t-test results
  ttest <- stats::t.test(x = data[[response]], mu = refvalue,
                         alternative = "two.sided",
                         conf.level = significance)

  mymean <- data_mean |> format_sigfig()
  mymeanconfint <- c(NA, NA)
  mymeanconfint[1] <- ttest$conf.int[1] |> format_sigfig()
  mymeanconfint[2] <- ttest$conf.int[2] |> format_sigfig()
  tvalue <- ttest$statistic |> abs() |> (\(x) sprintf("%.4f", x))()
  dof <- ttest$parameter
  tcritical <- stats::qt(alpha, dof) |> (\(x) sprintf("%.4f", x))()
  pvalue <- ttest$p.value |> (\(x) sprintf("%.4f", x))()

  # Being clear with some text
  h0_text <- sprintf("%s = %s", max_mean, min_mean)

  h1_text <- sprintf("%s \u2260 %s", max_mean, min_mean)

  positive <- "Il bias delle misure rispetto al valore di riferimento è statisticamente significativo"

  negative <- "Il bias delle misure rispetto al valore di riferimento non è statisticamente significativo"

  result <- ifelse(tvalue < tcritical, negative, positive)

  list(hypotheses = c("h0" = h0_text,
                      "h1" = h1_text),
       mean = c("mean" = mymean,
                "lwrci" = mymeanconfint[1],
                "uprci" = mymeanconfint[2]),
       test = c("dof" = dof |> as.character(),
                "alpha" = alpha |> (\(x) sprintf("%.3f", x))(),
                "tsper" = unname(tvalue),
                "ttheo" = tcritical,
                "pvalue" = pvalue),
       result = unname(result))

}

#' Plotly boxplots for a a serie of data, and for comparing the confidence interval
#' with a reference value and its extended uncertainty
#'
#' @description The function provides a simple {plotly} boxplot for a serie
#'  of data, and for comparing the confidence interval with a reference value
#'  and its extended uncertainty.
#'
#' @param data input data.frame with a column named *key* with progressive integers,
#' a column named *response* with the numeric values for the two groups and a
#' column named *outlier* with a logical vector.
#' @param response a character string with the label for the response numeric variable.
#' @param refval a reference numeric value.
#' @param refuncertainty the extended uncertainty for the reference numeric value.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {plotly} boxplot and an interval plot for comparing the confidence
#' interval of the supplied  measurement values with a reference value and
#' its extended uncertainty.
#'
#' @export
#'
#' @importFrom plotly plot_ly add_boxplot add_markers layout config subplot
boxplot_riprec <- function(data,
                           response,
                           refvalue,
                           refuncertainty,
                           udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.numeric(refvalue),
    is.numeric(refuncertainty),
    is.character(udm),
    colnames(data) %in% c("key", "outlier", "response"),
    dim(data)[2] == 3
  )

  cols <- ifelse(data$outlier == TRUE,
                 "#999999",
                 "black")

  ylabtitle <- paste0(response,
           ifelse(udm != "", paste0(" (", udm, ")"), ""))

  datanoutlier <- data[data$outlier == FALSE,]

  # 95% confidence interval for measurement values and the reference value
  reference_confint <- c(refvalue,
                         refvalue - refuncertainty,
                         refvalue + refuncertainty)

  mean_confint <- lm(datanoutlier$response ~ 1) |> confint()

  measurement_confint <- c(mean(datanoutlier$response, na.rm = TRUE),
                           mean_confint[1],
                           mean_confint[2])

  myconfint <- data.frame(label = c("misure", "riferimento"),
                          meanval = c(measurement_confint[1], reference_confint[1]),
                          lwrval = c(measurement_confint[2], reference_confint[2]),
                          uprval = c(measurement_confint[3], reference_confint[3])
  )

  # boxplot for measurement values
  myboxplot <- plotly::plot_ly() |>
    plotly::add_boxplot(
      data = datanoutlier,
      y = ~ response,
      x = "misure",
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
      x = "misure",
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
      yaxis = list(title = ylabtitle,
                   hoverformat = ".3s")
    )

  # comparing confidence intervals
  mycomparison <- plotly::plot_ly(
      data = myconfint,
      y = ~meanval ,
      x = ~label,
      name = "confint",
      type = "scatter",
      mode = "markers",
      color = I("#2780E3"),
      showlegend = FALSE,
      key = NULL,
      error_y = ~list(
        symmetric = FALSE,
        arrayminus = meanval - lwrval,
        array = uprval - meanval
      )
    )

  plotly::subplot(widths = c(0.3, 0.7),
                  myboxplot,
                  mycomparison,
                  shareY = TRUE,
                  which_layout = 1) |>
  plotly::layout(annotations = list(
      list(x = 0 , y = 1.01, text = "Boxplot delle misure", align = "left",
           showarrow = F, xref='paper', yref='paper'),
      list(x = 1 , y = 1.01, text = "Intervalli di confidenza delle medie", align = "right",
           showarrow = F, xref='paper', yref='paper'))
    ) |>
  plotly::config(displayModeBar = FALSE,
                 locale = "it")

}

#' ggplot2 boxplots for a a serie of data, and for comparing the confidence interval
#' with a reference value and its extended uncertainty
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
#' @param refval a reference numeric value.
#' @param refuncertainty the extended uncertainty for the reference numeric value.
#' @param udm a character string with the unit of measurement.
#'
#' @return A {ggplot2} boxplot and an interval plot for comparing the confidence
#' interval of the supplied  measurement values with a reference value and
#' its extended uncertainty.
#'
#' @export
#'
#' @import patchwork
#' @rawNamespace import(ggplot2, except = last_plot)
ggboxplot_riprec <- function(data,
                             response,
                             refvalue,
                             refuncertainty,
                             udm) {
  stopifnot(
    is.data.frame(data),
    is.character(response),
    is.numeric(refvalue),
    is.numeric(refuncertainty),
    is.character(udm)
  )

  rimosso <- NULL
  cols <- c("s\u00EC" = "#999999", "no" = "black")
  data$rimosso <- factor(data$rimosso, levels = c("s\u00EC", "no"))

  ylabtitle <- paste0(response, ifelse(udm != "", paste0(" (", udm, ")"), ""))

  quo_response <- ggplot2::ensym(response)

  # scale for the y axis
  enlarge <- 0.05 # proportion of increased axis lenght
  minvalue <- min(data[[response]], na.rm = TRUE)
  maxvalue <- max(data[[response]], na.rm = TRUE)
  yrange <- maxvalue - minvalue
  ymax <- maxvalue + enlarge * yrange
  ymin <- minvalue - enlarge * yrange

  datanoutlier <- data[which(data$rimosso == "no"),]

  # 95% confidence interval for measurement values and the reference value
  reference_confint <- c(refvalue,
                         refvalue - refuncertainty,
                         refvalue + refuncertainty)

  mean_confint <- lm(datanoutlier[[response]] ~ 1) |> confint()

  measurement_confint <- c(mean(datanoutlier[[response]], na.rm = TRUE),
                           mean_confint[1],
                           mean_confint[2])

  myconfint <- data.frame(label = c("misure", "riferimento"),
                          meanval = c(measurement_confint[1], reference_confint[1]),
                          lwrval = c(measurement_confint[2], reference_confint[2]),
                          uprval = c(measurement_confint[3], reference_confint[3])
  )

  myboxplot <- ggplot2::ggplot() +
    ggplot2::geom_boxplot(data = datanoutlier,
                          ggplot2::aes(x = "misure",
                                       y = !!quo_response),
                          fill = "white",
                          col = "black",
                          outlier.shape = NA) +
    ggplot2::geom_jitter(data = data,
                         ggplot2::aes(x = "misure",
                                      y = !!quo_response,
                                      col = rimosso),
                         width = 0.2) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax)) +
    ggplot2::labs(x = ggplot2::element_blank(),
                  y = ylabtitle,
                  title = "Boxplot delle misure") +
    ggplot2::scale_color_manual(values = cols,
                                breaks = c("s\u00EC", "no"),
                                labels = c("rimosso", "non rimosso"),
                                name = ggplot2::element_blank(),
                                drop = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "top")

  mycomparison <- ggplot2::ggplot(
    data = myconfint,
    ggplot2::aes(x = label,
                 y = meanval)
    ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = lwrval,
                   ymax = uprval),
      width = 0
      ) +
    ggplot2::scale_y_continuous(limits = c(ymin, ymax)) +
    ggplot2::labs(y = ggplot2::element_blank(),
                  x = ggplot2::element_blank(),
                  title = "Intervalli di confidenza delle medie") +
    ggplot2::theme_bw()

  myboxplot + mycomparison +
    patchwork::plot_layout(widths = c(1, 3), nrow = 1)

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

