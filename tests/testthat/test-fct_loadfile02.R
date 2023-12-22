test_that("Supported CSV files are correctly imported", {
  # exptected result
  expected_classes <- c(analita = "factor",
                        misura1 = "numeric",
                        misura2 = "numeric")

  # semicolon as field separator and comma as decimal separator
  sep_semicolon_decimal_comma <- system.file("extdata",
                                             "raw_sample_rip_deccomma.csv",
                                             package = "SIprecisa") |>
    csvimport() |>
    sapply("class")

  # semicolon as field separator and dot as decimal separator
  sep_semicolon_decimal_dot <- system.file("extdata",
                                             "raw_sample_rip.csv",
                                             package = "SIprecisa") |>
    csvimport() |>
    sapply("class")

  # semicolon as field separator and dot as decimal separator
  sep_comma_decimal_dot <- system.file("extdata",
                                        "raw_sample_rip_sepcomma.csv",
                                        package = "SIprecisa") |>
    csvimport() |>
    sapply("class")


  expect_identical(sep_semicolon_decimal_comma, expected_classes)
  expect_identical(sep_semicolon_decimal_dot, expected_classes)
  expect_identical(sep_comma_decimal_dot, expected_classes)
})


