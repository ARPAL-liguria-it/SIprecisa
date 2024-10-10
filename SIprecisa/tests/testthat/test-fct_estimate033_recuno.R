test_that("Errors are correctly handled for En-test on two values with
          extended uncertainty", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_entest_recuno(faildf, "c", "d"), "")
  expect_error(fct_entest_recuno(faildf, "c", "b", "c"), "")
})

# from ISO 13528:2022(E) Section E4
test_that("Calculations are correct for En-test when no differences are expected", {
  noeffectdata <- data.frame(myvalue = 0.037,
                             myuncertainty = 0.013)
  refv <- 0.044
  refu <- 0.0082

  noeffect_result <- fct_entest_recuno(noeffectdata, "myvalue", "myuncertainty",
                                       refvalue = refv, refuncertainty = refu)

  expect_equal(noeffect_result$hypotheses[[1]], "valore di riferimento = valore misurato")
  expect_equal(noeffect_result$hypotheses[[2]], "valore di riferimento ≠ valore misurato")
  expect_equal(noeffect_result$difference[[1]], "0.007000") # 0.0070000
  expect_equal(noeffect_result$difference[[2]], "-0.008370") # -0.008370 not reported
  expect_equal(noeffect_result$difference[[3]], "0.02237") # 0.02237 not reported
  expect_equal(noeffect_result$test[[1]], "0.4554") # 0.46
  expect_equal(noeffect_result$test[[2]], "1.000") # 1
})

test_that("Calculations are correct for En-test when a difference is expected", {
  # from ISO 13528:2022(E) Section E4
  yeseffectdata <- data.frame(myvalue = 0.0239,
                              myuncertainty = 0.0036)

  refv <- 0.044
  refu <- 0.0082

  yeseffect_result <- fct_entest_recuno(yeseffectdata, "myvalue", "myuncertainty",
                                        refvalue = refv, refuncertainty = refu)

  expect_equal(yeseffect_result$hypotheses[[1]], "valore di riferimento = valore misurato")
  expect_equal(yeseffect_result$hypotheses[[2]], "valore di riferimento ≠ valore misurato")
  expect_equal(yeseffect_result$difference[[1]], "0.02010") # 0.02010 not reported
  expect_equal(yeseffect_result$difference[[2]], "0.01114") # 0.01114 not reported
  expect_equal(yeseffect_result$difference[[3]], "0.02906") # 0.02906 not reported
  expect_equal(yeseffect_result$test[[1]], "2.244") #  2.24
  expect_equal(yeseffect_result$test[[2]], "1.000") # 1
})


test_that("ggboxplot_recuno", {
  yeseffectdata <- data.frame(myvalue = 0.0239,
                              myuncertainty = 0.0036)
  refv <- 0.044
  refu <- 0.0082

  testplot <- ggboxplot_recuno(yeseffectdata, "myvalue", "myuncertainty",
                               refvalue = refv, refuncertainty = refu,
                               "ug/L")

  expect_true(testplot |>
                ggplot2::is.ggplot())
  expect_equal(testplot$labels$y, "myvalue (ug/L)")
})

test_that("rowsummary_recuno works well", {
  yeseffectdata <- data.frame(myvalue = 0.0239,
                              myuncertainty = 0.0036)
  refv <- 0.044
  refu <- 0.0082

  mytbl <- rowsummary_recuno(yeseffectdata, "myvalue", "myuncertainty",
                             refvalue = refv, refuncertainty = refu, "mg/kg")

  expect_equal(mytbl$statistica |> unlist(), c("valore (mg/kg)", "incertezza estesa (mg/kg)"))
  expect_equal(colnames(mytbl), c("statistica", "misura", "riferimento"))
  expect_equal(mytbl[which(mytbl$statistica == "valore (mg/kg)"), "misura"], "0.02390")
  expect_equal(mytbl[which(mytbl$statistica == "valore (mg/kg)"), "riferimento"], "0.04400")
  expect_equal(mytbl[which(mytbl$statistica == "incertezza estesa (mg/kg)"), "misura"], "0.003600")
  expect_equal(mytbl[which(mytbl$statistica == "incertezza estesa (mg/kg)"), "riferimento"], "0.008200")
})
