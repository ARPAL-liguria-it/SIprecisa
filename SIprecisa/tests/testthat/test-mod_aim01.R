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
    session$setInputs(aim = "riprec",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "riprec")
    session$setInputs(aim = "rip",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "rip")
    session$setInputs(aim = "recuno",
                      nextbtn = TRUE,
                      yesbtn = TRUE)
    expect_true(r$aim01$aim == "recuno")
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

