test_that("Errors are correctly handled for t-test on 2 groups of data
          with one of them summarised by mean, standard deviation and number
          of values", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ttest_2samples_par(faildf, "c", "d"), "")
  expect_error(fct_ttest_2samples_par(faildf, "c", "b", alternative = "less"), "")
})

tomato_yields_a <- tomato_yields[fertilizer == "a"]
first_group <- "a"
first_n <- tomato_yields[fertilizer == "a", .N]
first_mean <- tomato_yields[fertilizer == "a", mean(pounds)]
first_sd <- tomato_yields[fertilizer == "a", sd(pounds)]
second_group <- "b"
second_n <- tomato_yields[fertilizer == "b", .N]
second_mean <- tomato_yields[fertilizer == "b", mean(pounds)]
second_sd <- tomato_yields[fertilizer == "b", sd(pounds)]

ttest_result1 <- fct_ttest_2samples_par(group1 = "a", mean1 = first_mean, sd1 = first_sd, n1 = first_n,
                                       group2 = "b", mean2 = second_mean, sd2 = second_sd, n2 = second_n)

# Results from Statistics for Experimenters - design, innovation, and discovery
# Second Edition, Wiley 2005. pag. 78 and 101
test_that("Calculations are correct for t-test on samples and alternative = different", {
  expect_equal(ttest_result1$hypotheses[[1]], "media di b = media di a")
  expect_equal(ttest_result1$hypotheses[[2]], "media di b ≠ media di a")
  expect_equal(ttest_result1$difference[[1]], "1.693")
  expect_equal(ttest_result1$difference[[2]], "-7.506")
  expect_equal(ttest_result1$difference[[3]], "10.89")
  expect_equal(ttest_result1$test[[1]], "7.3369")
  expect_equal(ttest_result1$test[[2]], "0.975")
  expect_equal(ttest_result1$test[[3]], "0.4313")
  expect_equal(ttest_result1$test[[4]], "2.3428")
  expect_equal(ttest_result1$test[[5]], "0.6787")
})

ttest_result2 <- fct_ttest_2samples_par(group1 = "a", mean1 = first_mean, sd1 = first_sd, n1 = first_n,
                                       group2 = "b", mean2 = second_mean, sd2 = second_sd, n2 = second_n,
                                       alternative = "greater")

test_that("Calculations are correct for t-test on 2 groups of values and alternative = greater", {
  expect_equal(ttest_result2$hypotheses[[1]], "media di b ≤ media di a")
  expect_equal(ttest_result2$hypotheses[[2]], "media di b > media di a")
  expect_equal(ttest_result2$difference[[1]], "1.693")
  expect_equal(ttest_result2$difference[[2]], "-5.695")
  expect_equal(ttest_result2$difference[[3]], "Inf")
  expect_equal(ttest_result2$test[[1]], "7.3369")
  expect_equal(ttest_result2$test[[2]], "0.950")
  expect_equal(ttest_result2$test[[3]], "0.4313")
  expect_equal(ttest_result2$test[[4]], "1.8816")
  expect_equal(ttest_result2$test[[5]], "0.3393")
})

ttest_result3 <- fct_ttest_2samples_par(group1 = "a", mean1 = first_mean, sd1 = first_sd, n1 = first_n,
                                       group2 = "b", mean2 = second_mean, sd2 = second_sd, n2 = second_n,
                                       significance = 0.99)
test_that("Calculations are correct for t-test on 2 groups of values and confidence = 0.99", {
  expect_equal(ttest_result3$hypotheses[[1]], "media di b = media di a")
  expect_equal(ttest_result3$hypotheses[[2]], "media di b ≠ media di a")
  expect_equal(ttest_result3$difference[[1]], "1.693")
  expect_equal(ttest_result3$difference[[2]], "-11.84")
  expect_equal(ttest_result3$difference[[3]], "15.22")
  expect_equal(ttest_result3$test[[1]], "7.3369")
  expect_equal(ttest_result3$test[[2]], "0.995")
  expect_equal(ttest_result3$test[[3]], "0.4313")
  expect_equal(ttest_result3$test[[4]], "3.4454")
  expect_equal(ttest_result3$test[[5]],  "0.6787")
})

# results from prospect C' and D' (pag. 39) of UNI ISO 2854:1988
ttest_result4 <- fct_ttest_2samples_par(group1 = "a", mean1 = 2.176, sd1 = sqrt(0.13960), n1 = 10,
                                        group2 = "b", mean2 = 2.520, sd2 = sqrt(0.12634), n2 = 12)
test_that("Calculations are correct for t-test on 2 groups of values", {
  expect_equal(ttest_result4$hypotheses[[1]], "media di b = media di a")
  expect_equal(ttest_result4$hypotheses[[2]], "media di b ≠ media di a")
  expect_equal(ttest_result4$difference[[1]], "0.3440")
  expect_equal(ttest_result4$difference[[2]], "0.01635")
  expect_equal(ttest_result4$difference[[3]], "0.6717")
  expect_equal(ttest_result4$test[[1]], "18.8992")
  expect_equal(ttest_result4$test[[2]], "0.975")
  expect_equal(ttest_result4$test[[3]], "2.1983")
  expect_equal(ttest_result4$test[[4]], "2.0938")
  expect_equal(ttest_result4$test[[5]],  "0.0406") # not reported on the reference
})

test_that("Errors are correctly handled for F-test on two groups of values", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ftest_2samples_par(faildf, "c", "d"), "")
  expect_error(fct_ftest_2samples_par(faildf, "c", "b", alternative = "less"), "")
})

fref_dt <- ftest_reference |> data.table::data.table()
fref_a <- fref_dt[group == "a"]
fgr_a <- "a"
fref_an <- fref_dt[group == "a", .N]
fref_asd <- fref_dt[group == "a", sd(value)]
fgr_b <- "b"
fref_bn <- fref_dt[group == "b", .N]
fref_bsd <- fref_dt[group == "b", sd(value)]

ftest_result1 <- fct_ftest_2samples_par(group1 = "a", sd1 = fref_asd, n1 = fref_an,
                                        group2 = "b", sd2 = fref_bsd, n2 = fref_bn)


# Results from Support of Microsoft Excel F.TEST function
# https://support.microsoft.com/en-us/office/
#   f-test-function-100a59e7-4108-46f8-8443-78ffacb6c0a7
test_that("Calculations are correct for f-test and alternative = different", {
  expect_equal(ftest_result1$hypotheses[[1]], "varianza di b = varianza di a")
  expect_equal(ftest_result1$hypotheses[[2]], "varianza di b ≠ varianza di a")
  expect_equal(ftest_result1$ratio[[1]], "1.628")
  expect_equal(ftest_result1$ratio[[2]], "0.1695")
  expect_equal(ftest_result1$ratio[[3]], "15.64")
  expect_equal(ftest_result1$test$dof, c("numeratore" = "4", "denominatore" = "4"))
  expect_equal(ftest_result1$test$alpha, "0.975")
  expect_equal(ftest_result1$test$fsper, "1.6281")
  expect_equal(ftest_result1$test$ftheo, "0.1041, 9.6045")
  expect_equal(ftest_result1$test$pvalue, "0.6483")
})

# results from prospect G and H of UNI ISO 2854:1988 (pag 40)
ftest_result2 <- fct_ftest_2samples_par(group1 = "a", sd1 = sqrt(0.13960), n1 = 10,
                                        group2 = "b", sd2 = sqrt(0.12634), n2 = 12)

test_that("Calculations are correct for f-test and alternative = different", {
  expect_equal(ftest_result2$hypotheses[[1]], "varianza di a = varianza di b")
  expect_equal(ftest_result2$hypotheses[[2]], "varianza di a ≠ varianza di b")
  expect_equal(ftest_result2$ratio[[1]], "1.105")
  expect_equal(ftest_result2$ratio[[2]], "0.3080") # 0.31
  expect_equal(ftest_result2$ratio[[3]], "4.323") # 4.4
  expect_equal(ftest_result2$test$dof, c("numeratore" = "9", "denominatore" = "11"))
  expect_equal(ftest_result2$test$alpha, "0.975")
  expect_equal(ftest_result2$test$fsper, "1.1050")
  expect_equal(ftest_result2$test$ftheo, "0.2556, 3.5879") # 0.25, 3.6
  expect_equal(ftest_result2$test$pvalue, "0.8612") # not reported
})



test_that("ggboxplot_2samples_par works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")
  test_amean <- testdata[rimosso == "no" & fertilizer == "a", mean(pounds)]
  test_asd <- testdata[rimosso == "no" & fertilizer == "a", sd(pounds)]
  test_an <- testdata[rimosso == "no" & fertilizer == "a", .N]
  test_bmean <- testdata[rimosso == "no" & fertilizer == "b", mean(pounds)]
  test_bsd <- testdata[rimosso == "no" & fertilizer == "b", sd(pounds)]
  test_bn <- testdata[rimosso == "no" & fertilizer == "b", .N]
  testsum <- data.frame(index = c("mean", "sd", "n"),
                        a = c(test_amean, test_asd, test_an),
                        b = c(test_amean, test_asd, test_an))

  expect_true(ggboxplot_2samples_par(testdata, "fertilizer", "pounds", "ug/L", "b", testsum) |>
                ggplot2::is.ggplot())
  expect_equal(ggboxplot_2samples_par(testdata, "fertilizer", "pounds", "ug/L", "b", testsum)$labels$x, "fertilizer")
  expect_equal(ggboxplot_2samples_par(testdata, "fertilizer", "pounds", "ug/L", "b", testsum)$labels$y, "pounds (ug/L)")
})

test_that("rowsummary_2samples_par works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")
  test_amean <- testdata[rimosso == "no" & fertilizer == "a", mean(pounds)]
  test_asd <- testdata[rimosso == "no" & fertilizer == "a", sd(pounds)]
  test_an <- testdata[rimosso == "no" & fertilizer == "a", .N]
  test_bmean <- tomato_yields[fertilizer == "b", mean(pounds)] |> round(3)
  test_bsd <- tomato_yields[fertilizer == "b", sd(pounds)] |> round(2)
  test_bn <- tomato_yields[fertilizer == "b", .N]

  res <- rowsummary_2samples_par(testdata, "pounds", "fertilizer", group2 = "b",
                                 mean2 = test_bmean, sd2 = test_bsd, n2 = test_bn,
                                 udm = "kg")

  expect_equal(res$statistica |> unlist(),
               c("n", "massimo (kg)", "media (kg)", "mediana (kg)", "minimo (kg)",
                 "deviazione standard (kg)"))
  expect_equal(colnames(res), c("statistica", "a", "b"))
  expect_equal(res[statistica == "media (kg)", a],
               sprintf("%.3g", tomato_yields[fertilizer == "a", mean(pounds)]))
  expect_equal(res[statistica == "massimo (kg)", b],
               "-")
  expect_equal(res[statistica == "mediana (kg)", b],
               "-")
  expect_equal(res[statistica == "n", a],
               tomato_yields[fertilizer == "a", .N] %>% as.character)
  expect_equal(res[statistica == "deviazione standard (kg)", b],
               "5.43")
})
