testServer(
  mod_aim01_server,
  # adding module params
  args = list(r = reactiveValues())
  , {

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # checking the output
    session$setInputs(aim = "2samples",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "2samples")
    session$setInputs(aim = "2samples_par",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "2samples_par")
    session$setInputs(aim = "1sample_mu",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "1sample_mu")
    session$setInputs(aim = "1sample_sigma",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "1sample_sigma")
    session$setInputs(aim = "2values_unc",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "2values_unc")
})

test_that("module aim01 ui works", {
  ui <- mod_aim01_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_aim01_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

