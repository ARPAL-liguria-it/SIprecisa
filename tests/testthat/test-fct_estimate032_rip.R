

test_that("ggboxplot_rip works well", {
  testdata <- tomato_yields[fertilizer == "a", .(parameter, pounds)]
  testdata$pounds_b <- tomato_yields[fertilizer == "b", .(pounds)]
  colnames(testdata)[2] <- "pounds_a"

  mytestplot <- ggboxplot_rip(testdata,
                                "pounds_a",
                                "pounds_b")

  expect_true(mytestplot |> ggplot2::is.ggplot())
  expect_equal(mytestplot$labels$y, "differenze relative (%)")
})

test_that("rowsummary_rip works well", {

  testdata <- data.table::data.table(uniiso_11352_b4)
  testdata$perc_diff <- testdata$rel_diff * 100
  testdata$outlier <- c(FALSE, FALSE, FALSE, TRUE, rep(FALSE, times = 6))
  mytesttable <- rowsummary_rip(testdata, "perc_diff", "%")

  expect_equal(mytesttable$statistica |> unlist(),
               c("n esclusi", "n", "massimo", "media", "mediana", "minimo"))
  expect_equal(colnames(mytesttable),
               c("statistica", "misure"))
  expect_equal(mytesttable[statistica == "media", differenze],
               sprintf("%.3g %", testdata[outlier == FALSE, mean(rel_diff)]))
  expect_equal(mytesttable[statistica == "massimo", differenze],
               sprintf("%.3g %", testdata[outlier == FALSE, max(rel_diff)]))
  expect_equal(mytesttable[statistica == "minimo", differenze],
               sprintf("%.3g %", testdata[outlier == FALSE, min(rel_diff)]))
  expect_equal(mytesttable[statistica == "mediana", differenze],
               sprintf("%.3g %", testdata[outlier == FALSE, stats::median(rel_diff)]))
  expect_equal(mytesttable[statistica == "n", differenze],
               testdata[outlier == FALSE, .N] |> as.character())
  expect_equal(mytesttable[statistica == "n esclusi", misure],
               testdata[outlier == TRUE, .N] |> as.character())
})
