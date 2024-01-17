test_that("signiftodigits works well", {
  expect_equal(sprintf("%.*f", signiftodigits(5, 4L), 5), "5.000")
  expect_equal(sprintf("%.*f", signiftodigits(0.5, 4L), 0.5), "0.5000")
  expect_equal(sprintf("%.*f", signiftodigits(0.501, 4L), 0.501), "0.5010")
  expect_equal(sprintf("%.*f", signiftodigits(0.00501, 4L), 0.00501), "0.005010")
  expect_equal(sprintf("%.*f", signiftodigits(0.00501, 2L), 0.00501), "0.0050")
  expect_equal(sprintf("%.*f", signiftodigits(Inf, 2L), Inf), "Inf")
})

test_that("htmltormarkdown works well", {
  expect_equal(htmltormarkdown("<h4> Test per la verifica della normalità (Shapiro-Wilk) </h4></br>"),
               "\n### Test per la verifica della normalità (Shapiro-Wilk)  \n  \n ")
})

test_that("get_gh_version works well", {
  expect_equal(get_gh_version("twbs", "bootstrap", tag = "v5.0.0"),
               "v5.0.0 del 2021-05-05")
})
