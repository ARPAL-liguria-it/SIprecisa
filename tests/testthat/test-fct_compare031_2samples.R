test_that("Errors are correctly handled for t-test on 2 groups of data", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ttest_2samples(faildf, "c", "d"), "")
  expect_error(fct_ttest_2samples(faildf, "c", "b", alternative = "less"), "")
})

# Results from Statistics for Experimenters - design, innovation, and discovery
# Second Edition, Wiley 2005. pag. 78 and 101
test_that("Calculations are correct for t-test on samples and alternative = different", {
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$hypotheses[[1]],
    "media di b = media di a"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$hypotheses[[2]],
    "media di b ≠ media di a"
  )
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$difference[[1]],
               "1.693")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$difference[[2]],
               "-7.506")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$difference[[3]],
               "10.89")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$test[[1]],
               "7.3369")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$test[[2]],
               "0.975")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$test[[3]],
               "0.4313")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$test[[4]],
               "2.3428")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer")$test[[5]],
               "0.6787")
})

test_that("Calculations are correct for t-test on 2 groups of values and alternative = greater", {
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$hypotheses[[1]],
    "media di b ≤ media di a"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$hypotheses[[2]],
    "media di b > media di a"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$difference[[1]],
    "1.693"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$difference[[2]], "-5.695"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$difference[[3]],
    "Inf"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$test[[1]],
    "7.3369"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$test[[2]],
    "0.950"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$test[[3]],
    "0.4313"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$test[[4]],
    "1.8816"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              alternative = "greater")$test[[5]],
    "0.3393"
  )
})

test_that("Calculations are correct for t-test on 2 groups of values and confidence = 0.99", {
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              significance = 0.99)$hypotheses[[1]],
    "media di b = media di a"
  )
  expect_equal(
    fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
              significance = 0.99)$hypotheses[[2]],
    "media di b ≠ media di a"
  )
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$difference[[1]],
               "1.693")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$difference[[2]],
               "-11.84")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$difference[[3]],
               "15.22")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$test[[1]],
               "7.3369")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$test[[2]],
               "0.995")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$test[[3]],
               "0.4313")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$test[[4]],
               "3.4454")
  expect_equal(fct_ttest_2samples(tomato_yields, "pounds", "fertilizer",
                         significance = 0.99)$test[[5]],
               "0.6787")
})

# prospect C' and D' of UNI ISO 2854:1988 (pag. 39)
test_that("Calculations are correct for t-test on 2 groups of values", {
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$hypotheses[[1]],
    "media di b = media di a"
  )
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$hypotheses[[2]],
    "media di b ≠ media di a"
  )
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$difference[[1]],
    "0.3440")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$difference[[2]],
               "0.01633")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$difference[[3]],
               "0.6716")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$test[[1]],
               "18.8994")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$test[[2]],
               "0.975")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$test[[3]],
               "2.1982")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$test[[4]],
               "2.0938")
  expect_equal(
    fct_ttest_2samples(uniiso_2854_x, response = "value", group = "group")$test[[5]],
               "0.0406")
})

test_that("Errors are correctly handled for F-test on two groups of values", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ftest_2samples(faildf, "c", "d"), "")
  expect_error(fct_ftest_2samples(faildf, "c", "b", alternative = "less"), "")
})

# Results from Support of Microsoft Excel F.TEST function
# https://support.microsoft.com/en-us/office/
#   f-test-function-100a59e7-4108-46f8-8443-78ffacb6c0a7
test_that("Calculations are correct for f-test and alternative = different", {
  expect_equal(
    fct_ftest_2samples(ftest_reference, "value", "group")$hypotheses[[1]],
    "varianza di b = varianza di a"
  )
  expect_equal(
    fct_ftest_2samples(ftest_reference, "value", "group")$hypotheses[[2]],
    "varianza di b ≠ varianza di a"
  )
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$ratio[[1]],
               "1.628")
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$ratio[[2]],
               "0.1695")
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$ratio[[3]],
               "15.64")
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$test$dof,
               c("numeratore" = "4", "denominatore" = "4"))
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$test$alpha,
                "0.975")
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$test$fsper,
               "1.6281")
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$test$ftheo,
               "0.1041, 9.6045")
  expect_equal(fct_ftest_2samples(ftest_reference, "value", "group")$test$pvalue,
               "0.6483")
})

# results from prospect G and H of UNI ISO 2854:1988 (pag. 40)
test_that("Calculations are correct for f-test and alternative = different", {
  expect_equal(
    fct_ftest_2samples(uniiso_2854_x, "value", "group")$hypotheses[[1]],
    "varianza di a = varianza di b"
  )
  expect_equal(
    fct_ftest_2samples(uniiso_2854_x, "value", "group")$hypotheses[[2]],
    "varianza di a ≠ varianza di b"
  )
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$ratio[[1]],
               "1.105") # 1.10
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$ratio[[2]],
               "0.3080") # 0.31
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$ratio[[3]],
               "4.322") # 4.4
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$test$dof,
               c("numeratore" = "9", "denominatore" = "11"))
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$test$alpha,
               "0.975")
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$test$fsper,
               "1.1049")
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$test$ftheo,
               "0.2556, 3.5879") # 0.25 e 3.6
  expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group")$test$pvalue,
               "0.8613") # not reported
})

test_that("Calculations are correct for f-test and alternative = different
            significance = 0.99", {
expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group",
                                significance = 0.99)$ratio[[2]],
             "0.1996") # 0.20
expect_equal(fct_ftest_2samples(uniiso_2854_x, "value", "group",
                                significance = 0.99)$ratio[[3]],
             "6.977") # 7.0
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

test_that("ggboxplot_2samples works well", {
  testdata <- tomato_yields
  testdata$rimosso <- c(rep("no", 8), "sì", "no", "no")

  expect_true(ggboxplot_2samples(testdata, "fertilizer", "pounds", "ug/L") |> ggplot2::is.ggplot())
  expect_equal(ggboxplot_2samples(testdata, "fertilizer", "pounds", "ug/L")$labels$x, "fertilizer")
  expect_equal(ggboxplot_2samples(testdata, "fertilizer", "pounds", "ug/L")$labels$y, "pounds (ug/L)")
})

test_that("rowsummary_2samples works well", {
  expect_equal(rowsummary_2samples(tomato_yields, "pounds", "fertilizer", "kg")$statistica |>
                 unlist(),
               c("n", "massimo (kg)", "media (kg)", "mediana (kg)", "minimo (kg)",
                 "deviazione standard (kg)"))
  expect_equal(colnames(rowsummary_2samples(tomato_yields, "pounds", "fertilizer")),
               c("statistica", "a", "b"))
  expect_equal(rowsummary_2samples(tomato_yields, "pounds", "fertilizer")[statistica == "media", a],
               sprintf("%.3g", tomato_yields[fertilizer == "a", mean(pounds)]))
  expect_equal(rowsummary_2samples(tomato_yields, "pounds", "fertilizer")[statistica == "massimo", b],
               sprintf("%.3g", tomato_yields[fertilizer == "b", max(pounds)]))
  expect_equal(rowsummary_2samples(tomato_yields, "pounds", "fertilizer")[statistica == "mediana", b],
               "24.0")
  expect_equal(rowsummary_2samples(tomato_yields, "pounds", "fertilizer")[statistica == "n", a],
               tomato_yields[fertilizer == "a", .N] %>% as.character)
  expect_equal(rowsummary_2samples(tomato_yields, "pounds", "fertilizer")[statistica == "deviazione standard", b],
               "5.43")
})
