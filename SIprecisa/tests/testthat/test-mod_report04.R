r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    compare03 = reactiveValues())

testServer(
  mod_report04_server,
  # Add here your module params
  args = list(r), {

    r$aim01$aim <- "1sample_sigma"
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$data <- tomato_yields
    r$compare03$myparameter <- "yield"
    r$compare03$yield$normality <- "myresult"
    r$compare03$yield$ttest <- NA
    r$compare03$yield$ftest <- "ftestresult"
    r$compare03$yield$saved <- TRUE
    r$compare03$saved_flag <- 1
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # Testing the setting of inputs
    session$setInputs(title = "My nice title")
    expect_true(input$title == "My nice title")

    session$setInputs(description = "My very long description")
    expect_true(input$description == "My very long description")
    session$setInputs(discussion = "My very in depth discussion")
    expect_true(input$discussion == "My very in depth discussion")

    session$setInputs(content = c("shapirotest", "ftest"))
    expect_equal(input$content, c("shapirotest", "ftest"))

    expect_true(inherits(session$getReturned(), "shiny.render.function"))
})

test_that("module ui works", {
  ui <- mod_report04_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_report04_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

