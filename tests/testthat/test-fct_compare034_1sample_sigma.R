test_that("Errors are correctly handled for chi^2-test on 1 group of data
          with vs a reference value", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_chitest_1sample_sigma(faildf, "c", "d"), "")
  expect_error(fct_chitest_1sample_sigma(faildf, "c", "b", alternative = "less"), "")
})


tomato_yields_a <- tomato_yields[fertilizer == "a"]
ref_sd <- 20.83

chitest_result1 <- fct_chitest_1sample_sigma(tomato_yields_a, "pounds", "fertilizer", "ref", ref_sd)

# comparison vs EnvStats::varTest(tomato_yields_a$pounds, sigma.squared = ref_sd^2)
test_that("Calculations are correct for chi^2-test on one sample and alternative = different", {
  expect_equal(chitest_result1$hypotheses[[1]], "varianza di a = varianza di riferimento ref")
  expect_equal(chitest_result1$hypotheses[[2]], "varianza di a ≠ varianza di riferimento ref")
  expect_equal(chitest_result1$ratio[[1]], "7.246") # 52.498^0.5
  expect_equal(chitest_result1$ratio[[2]], "4.341") # 8.84471^0.5
  expect_equal(chitest_result1$ratio[[3]], "20.82") # 433.49289^0.5
  expect_equal(chitest_result1$test[[1]], "4")
  expect_equal(chitest_result1$test[[2]], "0.975")
  expect_equal(chitest_result1$test[[3]], "0.4840") # 0.4838525
  expect_equal(chitest_result1$test[[4]], "0.4844, 11.1433") # not reported
  expect_equal(chitest_result1$test[[5]], "0.0499") # 0.04989244
})


chitest_result2 <- fct_chitest_1sample_sigma(tomato_yields_a, "pounds", "fertilizer",
                                           "ref", ref_sd, alternative = "greater")

# comparison vs EnvStats::varTest(tomato_yields_a$pounds,
# sigma.squared = ref_sd^2, alternative = "greater)
test_that("Calculations are correct for chi^2-test on one sample and alternative = greater", {
  expect_equal(chitest_result2$hypotheses[[1]], "varianza di a ≤ varianza di riferimento ref")
  expect_equal(chitest_result2$hypotheses[[2]], "varianza di a > varianza di riferimento ref")
  expect_equal(chitest_result2$ratio[[1]], "7.246") # 52.498^0.5
  expect_equal(chitest_result2$ratio[[2]], "4.705") # 22.13301^0.5
  expect_equal(chitest_result2$ratio[[3]], "Inf") # Inf
  expect_equal(chitest_result2$test[[1]], "4")
  expect_equal(chitest_result2$test[[2]], "0.950")
  expect_equal(chitest_result2$test[[3]], "0.4840") # 0.4838525
  expect_equal(chitest_result2$test[[4]], "9.4877") # not reported
  expect_equal(chitest_result2$test[[5]], "0.9750") # 0.9750538
})

ref_sd2 <- 4.34
chitest_result3 <- fct_chitest_1sample_sigma(tomato_yields_a, "pounds", "fertilizer",
                                             "ref", ref_sd2, alternative = "greater")
# comparison vs EnvStats::varTest(tomato_yields[fertilizer == "a", pounds],
# sigma.squared = 4.34^2, alternative = "greater")
test_that("Calculations are correct for chi^2-test on one sample and alternative greater v2", {
  expect_equal(chitest_result3$hypotheses[[1]], "varianza di a ≤ varianza di riferimento ref")
  expect_equal(chitest_result3$hypotheses[[2]], "varianza di a > varianza di riferimento ref")
  expect_equal(chitest_result3$ratio[[1]], "7.246")
  expect_equal(chitest_result3$ratio[[2]], "4.705")
  expect_equal(chitest_result3$ratio[[3]], "Inf")
  expect_equal(chitest_result3$test[[1]], "4")
  expect_equal(chitest_result3$test[[2]], "0.950")
  expect_equal(chitest_result3$test[[3]], "11.1487") # 11.14868
  expect_equal(chitest_result3$test[[4]], "9.4877") # not reported
  expect_equal(chitest_result3$test[[5]], "0.0249") # 0.02494295
})

ref_sd4 <- sqrt(0.0900)
chitest_result4 <- fct_chitest_1sample_sigma(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                             "value", "group",
                                             "iso", ref_sd4,
                                             alternative = "greater")

# results at prospect E UNI ISO 2854:1988 (pag. 40)
test_that("Calculations are correct for chi^2-test on one sample and alternative greater v2", {
  expect_equal(chitest_result4$hypotheses[[1]], "varianza di a ≤ varianza di riferimento iso")
  expect_equal(chitest_result4$hypotheses[[2]], "varianza di a > varianza di riferimento iso")
  expect_equal(chitest_result4$ratio[[1]], "0.3736")
  expect_equal(chitest_result4$ratio[[2]], "0.2725") # not reported
  expect_equal(chitest_result4$ratio[[3]], "Inf") # not reported
  expect_equal(chitest_result4$test[[1]], "9")
  expect_equal(chitest_result4$test[[2]], "0.950")
  expect_equal(chitest_result4$test[[3]], "13.9596") # 13.96
  expect_equal(chitest_result4$test[[4]], "16.9190") # 16.92
  expect_equal(chitest_result4$test[[5]], "0.1238") # not reported
})

chitest_result5 <- fct_chitest_1sample_sigma(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                             "value", "group",
                                             "iso", ref_sd4)
# results at prospect F UNI ISO 2854:1988 (pag. 40)
expect_equal(chitest_result5$ratio[[2]], "0.2570") # 0.257
expect_equal(chitest_result5$ratio[[3]], "0.6821") # 0.682

chitest_result6 <- fct_chitest_1sample_sigma(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                             "value", "group",
                                             "iso", ref_sd4,
                                             significance = 0.99)
# results at prospect F UNI ISO 2854:1988 (pag. 40)
expect_equal(chitest_result6$ratio[[2]], "0.2308") # 0.231
expect_equal(chitest_result6$ratio[[3]], "0.8510") # 0.851


test_that("ggboxplot_1sample_sigma works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")

  expect_true(ggboxplot_1sample_sigma(testdata, "fertilizer", "pounds", "law", 4.34, "ug/L") |>
                ggplot2::is.ggplot())
  expect_equal(ggboxplot_1sample_sigma(testdata, "fertilizer", "pounds", "law", 4.34, "ug/L")$labels$x, "fertilizer")
  expect_equal(ggboxplot_1sample_sigma(testdata, "fertilizer", "pounds", "law", 4.34, "ug/L")$labels$y, "pounds (ug/L)")
})

test_that("rowsummary_1sample_sigma works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")

  res <- rowsummary_1sample_sigma(testdata, "pounds", "fertilizer",
                                  "law", 4.34, udm = "kg")

  expect_equal(res$statistica |> unlist(),
               c("n", "massimo (kg)", "media (kg)", "mediana (kg)", "minimo (kg)",
                 "deviazione standard (kg)"))
  expect_equal(colnames(res), c("statistica", "a", "law"))
  expect_equal(res[statistica == "media (kg)", a],
               sprintf("%.3g", tomato_yields[fertilizer == "a", mean(pounds)]))
  expect_equal(res[statistica == "media (kg)", law], "-")
  expect_equal(res[statistica == "massimo (kg)", law],
               "-")
  expect_equal(res[statistica == "mediana (kg)", law],
               "-")
  expect_equal(res[statistica == "n", a],
               tomato_yields[fertilizer == "a", .N] %>% as.character)
  expect_equal(res[statistica == "deviazione standard (kg)", law],
               "4.34")
})
