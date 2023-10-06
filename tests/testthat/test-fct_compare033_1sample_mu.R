test_that("Errors are correctly handled for t-test on 1 group of data
          with vs a reference value", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_ttest_1sample_mu(faildf, "c", "d"), "")
  expect_error(fct_ttest_1sample_mu(faildf, "c", "b", alternative = "less"), "")
})

# from prospect A' and B' of UNI ISO 2854:1988 (pag. 37)
ttest_result1 <- fct_ttest_1sample_mu(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                      response = "value", group = "group",
                                      reflabel = "iso", reference = 2.40)

test_that("Calculations are correct for t-test on one sample and alternative = different", {
  expect_equal(ttest_result1$hypotheses[[1]], "valore di riferimento iso = media di a")
  expect_equal(ttest_result1$hypotheses[[2]], "valore di riferimento iso ≠ media di a")
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
ttest_result2 <- fct_ttest_1sample_mu(uniiso_2854_x[which(uniiso_2854_x$group == "a"), ],
                                      response = "value", group = "group",
                                      reflabel = "iso", reference = 2.40,
                                      significance = 0.99)

test_that("Calculations are correct for t-test on one sample and confidence = 0.99", {
  expect_equal(ttest_result2$hypotheses[[1]], "valore di riferimento iso = media di a")
  expect_equal(ttest_result2$hypotheses[[2]], "valore di riferimento iso ≠ media di a")
  expect_equal(ttest_result2$mean[[1]], "2.176")
  expect_equal(ttest_result2$mean[[2]], "1.792")
  expect_equal(ttest_result2$mean[[3]], "2.560")
  expect_equal(ttest_result2$test[[1]], "9")
  expect_equal(ttest_result2$test[[2]], "0.995")
  expect_equal(ttest_result2$test[[3]], "1.8950") # not reported on the reference
  expect_equal(ttest_result2$test[[4]], "3.2498")
  expect_equal(ttest_result2$test[[5]], "0.0906") # not reported on the reference
})


test_that("ggboxplot_1sample_mu works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")

  expect_true(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L") |>
                ggplot2::is.ggplot())
  expect_equal(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L")$labels$x, "fertilizer")
  expect_equal(ggboxplot_1sample_mu(testdata, "fertilizer", "pounds", "law", 30, "ug/L")$labels$y, "pounds (ug/L)")
})

test_that("rowsummary_1sample_mu works well", {
  testdata <- tomato_yields[fertilizer == "a"]
  testdata$rimosso <- c(rep("no", 3), "sì", "no")

  res <- rowsummary_1sample_mu(testdata, "pounds", "fertilizer",
                                 "law", 30, udm = "kg")

  expect_equal(res$statistica |> unlist(),
               c("n", "massimo (kg)", "media (kg)", "mediana (kg)", "minimo (kg)",
                 "deviazione standard (kg)"))
  expect_equal(colnames(res), c("statistica", "a", "law"))
  expect_equal(res[statistica == "media (kg)", a],
               sprintf("%.3g", tomato_yields[fertilizer == "a", mean(pounds)]))
  expect_equal(res[statistica == "media (kg)", law], "30")
  expect_equal(res[statistica == "massimo (kg)", law],
               "-")
  expect_equal(res[statistica == "mediana (kg)", law],
               "-")
  expect_equal(res[statistica == "n", a],
               tomato_yields[fertilizer == "a", .N] %>% as.character)
  expect_equal(res[statistica == "deviazione standard (kg)", law],
               "-")
})
