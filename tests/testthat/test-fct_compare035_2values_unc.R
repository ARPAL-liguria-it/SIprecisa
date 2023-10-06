test_that("Errors are correctly handled for En-test on two values with
          extended uncertainty", {
  faildf <- data.frame(a = "a",
                       b = "b",
                       c = 3)
  expect_error(fct_entest_2values_unc(faildf, "c", "d"), "")
  expect_error(fct_entest_2values_unc(faildf, "c", "b", "c"), "")
})


# from ISO 13528:2022(E) Section E4
noeffectdata <- data.frame(mygroup = c("L03", "pt"),
                           myvalue = c(0.037, 0.044),
                           myuncertainty = c(0.013, 0.0082))

noeffect_result <- fct_entest_2values_unc(noeffectdata, "myvalue", "myuncertainty", "mygroup")

test_that("Calculations are correct for En-test when no differences are expected", {
  expect_equal(noeffect_result$hypotheses[[1]], "pt = L03")
  expect_equal(noeffect_result$hypotheses[[2]], "pt ≠ L03")
  expect_equal(noeffect_result$difference[[1]], "0.007000") # 0.0070000
  expect_equal(noeffect_result$difference[[2]], "-0.008370") # -0.008370 not reported
  expect_equal(noeffect_result$difference[[3]], "0.02237") # 0.02237 not reported
  expect_equal(noeffect_result$test[[1]], "0.4554") # 0.46
  expect_equal(noeffect_result$test[[2]], "1.000") # 1
})

# from ISO 13528:2022(E) Section E4
yeseffectdata <- data.frame(mygroup = c("L12", "pt"),
                            myvalue = c(0.0239, 0.044),
                            myuncertainty = c(0.0036, 0.0082))

yeseffect_result <- fct_entest_2values_unc(yeseffectdata, "myvalue", "myuncertainty", "mygroup")

test_that("Calculations are correct for En-test when a difference is expected", {
  expect_equal(yeseffect_result$hypotheses[[1]], "pt = L12")
  expect_equal(yeseffect_result$hypotheses[[2]], "pt ≠ L12")
  expect_equal(yeseffect_result$difference[[1]], "0.02010") # 0.02010 not reported
  expect_equal(yeseffect_result$difference[[2]], "0.01114") # 0.01114 not reported
  expect_equal(yeseffect_result$difference[[3]], "0.02906") # 0.02906 not reported
  expect_equal(yeseffect_result$test[[1]], "2.244") #  2.24
  expect_equal(yeseffect_result$test[[2]], "1.000") # 1
})


test_that("ggboxplot_2values_unc", {
  expect_true(ggboxplot_2values_unc(yeseffectdata, "mygroup", "myvalue", "myuncertainty", "ug/L") |>
                ggplot2::is.ggplot())
  expect_equal(ggboxplot_2values_unc(yeseffectdata, "mygroup", "myvalue", "myuncertainty", "ug/L")$labels$x, "mygroup")
  expect_equal(ggboxplot_2values_unc(yeseffectdata, "mygroup", "myvalue", "myuncertainty", "ug/L")$labels$y, "myvalue (ug/L)")
})

test_that("rowsummary_2values_unc works well", {
  mytbl <- rowsummary_2values_unc(yeseffectdata, "myvalue", "myuncertainty", "mygroup", "mg/kg")

  expect_equal(mytbl$statistica |> unlist(), c("valore (mg/kg)", "incertezza estesa (mg/kg)"))
  expect_equal(colnames(mytbl), c("statistica", "L12", "pt"))
  expect_equal(mytbl[which(mytbl$statistica == "valore (mg/kg)"), "L12"], "0.02390")
  expect_equal(mytbl[which(mytbl$statistica == "valore (mg/kg)"), "pt"], "0.04400")
  expect_equal(mytbl[which(mytbl$statistica == "incertezza estesa (mg/kg)"), "L12"], "0.003600")
  expect_equal(mytbl[which(mytbl$statistica == "incertezza estesa (mg/kg)"), "pt"], "0.008200")
})
