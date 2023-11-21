test_that("Errors are correctly handled for t-test with a reference value", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ttest_riprec(faildf, "c", "d"), "")
  expect_error(fct_ttest_riprec(faildf, "c", "b", alternative = "less"), "")
})

# from prospect A' and B' of UNI ISO 2854:1988 (pag. 37)
ttest_result1 <- fct_ttest_riprec(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                      response = "value",
                                      refvalue = 2.40)

test_that("Calculations are correct for t-test on one sample and alternative = different", {
  expect_equal(ttest_result1$hypotheses[[1]], "valore di riferimento = media delle misure")
  expect_equal(ttest_result1$hypotheses[[2]], "valore di riferimento ≠ media delle misure")
  expect_equal(ttest_result1$mean[[1]], "2.176")
  expect_equal(ttest_result1$mean[[2]], "1.909")
  expect_equal(ttest_result1$mean[[3]], "2.443")
  expect_equal(ttest_result1$test[[1]], "9")
  expect_equal(ttest_result1$test[[2]], "0.975")
  expect_equal(ttest_result1$test[[3]], "1.8950") # not reported on the reference
  expect_equal(ttest_result1$test[[4]], "2.2622")
  expect_equal(ttest_result1$test[[5]], "0.0906") # not reported on the reference
})

# from prospect B' of UNI ISO 2854:1988 (pag. 37)
ttest_result2 <- fct_ttest_riprec(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                      response = "value",
                                      refvalue = 2.40,
                                      significance = 0.99)

test_that("Calculations are correct for t-test on one sample and confidence = 0.99", {
  expect_equal(ttest_result2$hypotheses[[1]], "valore di riferimento = media delle misure")
  expect_equal(ttest_result2$hypotheses[[2]], "valore di riferimento ≠ media delle misure")
  expect_equal(ttest_result2$mean[[1]], "2.176")
  expect_equal(ttest_result2$mean[[2]], "1.792")
  expect_equal(ttest_result2$mean[[3]], "2.560")
  expect_equal(ttest_result2$test[[1]], "9")
  expect_equal(ttest_result2$test[[2]], "0.995")
  expect_equal(ttest_result2$test[[3]], "1.8950") # not reported on the reference
  expect_equal(ttest_result2$test[[4]], "3.2498")
  expect_equal(ttest_result2$test[[5]], "0.0906") # not reported on the reference
})

entest_result <- fct_entest_riprec(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                  response = "value",
                                  refvalue = 2.40,
                                  refuncertainty = 0.05)

meas_mean <- uniiso_2854_x[which(uniiso_2854_x$group == "a"), "value"] |> mean()
meas_sd <- uniiso_2854_x[which(uniiso_2854_x$group == "a"), "value"] |> sd()
meas_n <- uniiso_2854_x[which(uniiso_2854_x$group == "a"), "value"] |> length()
meas_pm <- qt(0.975, meas_n - 1) * meas_sd / sqrt(meas_n)
meas_ref <- data.frame(valore = meas_mean,
                       incertezza = meas_pm)

entest_ref <- fct_entest_recuno(data = meas_ref,
                                response = "valore",
                                uncertainty = "incertezza",
                                refvalue = 2.40,
                                refuncertainty = 0.05)

test_that("En calculations are correct", {
  expect_equal(entest_result$difference, entest_ref$difference)
  expect_equal(entest_result$test, entest_ref$test)
})

# Results from An analysis of variance test for normality (complete samples),
#'  Biometrika (1965), 52, 3 and 2, p. 591.
#'  Section 4 - Examples, pag. 606, Example 1.
test_that("Calculations are correct for Shapiro-Wilk test", {
  expect_equal(
    fct_shapiro(shapirotest_reference)$W %>% round(2),
    0.79
  )
  expect_true(
    fct_shapiro(shapirotest_reference)$pvalue %>% round(3) < 0.01
  )
  expect_equal(
    fct_shapiro(shapirotest_reference)$result,
    "I valori non sono compatibili con una distribuzione normale"
  )
})

# Results from  Tietjen and Moore (August 1972),
# Some Grubbs-Type Statistics for the Detection of Outliers,
# Technometrics, 14(3), pp. 583-597. Also available at
# https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h1.htm
test_that("Errors are correctly handled for GESD outlier test", {
  faildf <- data.frame(a = c(1, 2))
  faildf1 <- data.frame(a = 1:5)
  expect_error(fct_gesd(faildf$a), "")
  expect_error(fct_GESD(faildf1$a, signif = 0.05), "")
})

test_that("Calculations are correct for GESD outlier test", {
  expect_equal(
    fct_gesd(uranium_cps, 0.99)$data$I[which(fct_gesd(uranium_cps, 0.99)$data$outlier== TRUE)],
    245.57
  )
  expect_equal(
    fct_gesd(uranium_cps, 0.99)$text,
    "245.57 è un possibile valore anomalo"
  )
})

# Results from UNI ISO 16269-4:2019 - Statistical interpretation of data - Part 4:
# Detection and treatment of outliers. Section 4.3.2.
test_that("Calculations are correct for GESD outlier test", {
  expect_equal(
    fct_gesd(uniiso_16269_4_432)$data$I[1:3],
    c(12.60, 5.80, -2.21)
  )
  expect_equal(
    fct_gesd(uniiso_16269_4_432)$data$R[1:3] %>% round(4),
    c(3.6559, 3.2634, 2.1761)
  )
  expect_equal(
    fct_gesd(uniiso_16269_4_432)$data$lambda[1:3] %>% round(4),
    c(2.7058, 2.6785, 2.6492)
  )
  expect_equal(
    fct_gesd(uniiso_16269_4_432)$data$outlier[1:3],
    c(TRUE, TRUE, FALSE)
  )
  expect_equal(
    fct_gesd(uniiso_16269_4_432)$text,
    "12.6, 5.8 sono possibili valori anomali"
  )
})

test_that("ggboxplot_riprec works well", {
  testdata <- tomato_yields[fertilizer == "a", .(parameter, pounds)]
  testdata$rimosso <- c(rep("no", 2), "sì", rep("no", 2))
  refb <- tomato_yields[fertilizer == "b", mean(pounds)]
  uncb <- tomato_yields[fertilizer == "b",
                        qt(0.975, length(pounds)-1) *
                          sd(pounds)/sqrt(length(pounds))]

  mytestplot1 <- ggboxplot_riprec(testdata,
                                 "pounds",
                                 refvalue = refb,
                                 refuncertainty = uncb,
                                 conflevel = 0.95,
                                 "ug/L")

  expect_equal(mytestplot1 |> length(), 2)
  expect_true(mytestplot1 |> ggplot2::is.ggplot())
  expect_equal(mytestplot1[[1]]$labels$y, "pounds (ug/L)")

  mytestplot2 <- ggboxplot_riprec(testdata,
                                  "pounds",
                                  refvalue = 0,
                                  refuncertainty = uncb,
                                  conflevel = 0.95,
                                  "ug/L")

  expect_true(mytestplot2 |> ggplot2::is.ggplot())
  expect_equal(mytestplot2$labels$y, "pounds (ug/L)")
})

test_that("rowsummary_riprec works well", {

  testdata <- tomato_yields[fertilizer == "a",
                            .(parameter, pounds)]
  testdata$outlier <- c(FALSE, FALSE, FALSE, TRUE, FALSE)
  mytesttable <- rowsummary_riprec(testdata, "pounds", "kg")

  expect_equal(mytesttable$statistica |> unlist(),
               c("n esclusi", "n", "massimo", "media", "mediana", "minimo"))
  expect_equal(colnames(mytesttable),
               c("statistica", "misure"))
  expect_equal(mytesttable[statistica == "media", misure],
               sprintf("%.3g kg", testdata[outlier == FALSE, mean(pounds)]))
  expect_equal(mytesttable[statistica == "massimo", misure],
               sprintf("%.3g kg", testdata[outlier == FALSE, max(pounds)]))
  expect_equal(mytesttable[statistica == "minimo", misure],
               sprintf("%.3g kg", testdata[outlier == FALSE, min(pounds)]))
  expect_equal(mytesttable[statistica == "mediana", misure],
               sprintf("%.3g kg", testdata[outlier == FALSE, stats::median(pounds)]))
  expect_equal(mytesttable[statistica == "n", misure],
               testdata[outlier == FALSE, .N] |> as.character())
  expect_equal(mytesttable[statistica == "n esclusi", misure],
               testdata[outlier == TRUE, .N] |> as.character())
})

test_that("fct_trueness_riprec works well", {

  testres <- fct_trueness_riprec(uniiso_5725_4_b3, "value", 0.403)

  expect_named(testres,
               c("alpha", "n", "mean", "lwr", "upr", "recovery", "bias",
                 "bias_rms", "relative_bias"))
  expect_equal(testres$alpha |> format_sigfig(3L),
               "0.950")
  expect_equal(testres$n |> format_sigfig(0L),
               "12")
  expect_equal(testres$mean |> format_sigfig(4L),
               "0.4021")
  expect_equal(testres$bias |> format_sigfig(2L),
               "-0.00092")
  expect_equal(testres$relative_bias |> format_sigfig(2L),
               "-0.23") # not in reference
  expect_equal(testres$recovery |> format_sigfig(3L),
               "99.8") # not in reference
})

test_that("fct_precision_riprec works well", {

  testres <- fct_precision_riprec(uniiso_11352_b1, "value")

  expect_named(testres,
               c("alpha", "devstd", "n", "repeatability", "rel_repeatability", "rsd"))
  expect_equal(testres$alpha |> format_sigfig(3L),
               "0.975")
  expect_equal(testres$n |> format_sigfig(0L),
               "30")
  expect_equal(testres$devstd |> format_sigfig(3L),
               "0.122")
  expect_equal(testres$rsd |> format_sigfig(3L),
               "5.21")
  expect_equal(testres$repeatability |> format_sigfig(3L),
               "0.352") # not in reference
  expect_equal(testres$rel_repeatability |> format_sigfig(3L),
               "15.1") # not in reference
})
