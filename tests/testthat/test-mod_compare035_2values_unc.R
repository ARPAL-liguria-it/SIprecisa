r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    compare03 = reactiveValues())

effectdata <- data.table::data.table(
  myparameter = c("noeff", "noeff", "yeseff", "yeseff"),
  mygroup = rep(letters[1:2], 2),
  myvalue = c(2.14, 4.85, 4.84, 2.15),
  myuncertainty = c(1.02, 2.50, 2.30, 1.02))

testServer(
  mod_compare035_2values_unc_server,
  # Add here your module params
  args = list(r), {

    r$loadfile02$parvar <- "myparameter"
    r$loadfile02$responsevar <- "myvalue"
    r$loadfile02$uncertaintyvar <- "myuncertainty"
    r$loadfile02$groupvar <- "mygroup"
    r$loadfile02$data <- effectdata
    r$compare03$myparameter <- "yeseff"
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    session$setInputs(udm = "ug/L")
    session$flushReact()
    expect_true(input$udm == "ug/L")

    # testing the intermediate dataset
    expect_equal(dim(input_data()), c(2, 3))
    expect_equal(colnames(input_data()), c("response", "uncertainty", "group"))
    expect_equal(input_data()$response, c(4.84, 2.15))
    expect_equal(input_data()$uncertainty, c(2.30, 1.02))
    expect_equal(input_data()$group, c("a", "b"))

    # Testing the outputs
    ## Testing the boxplot output
    expect_true(inherits(output$boxplot, "json"))
    ## Testing the summary output
    expect_true(inherits(output$summarytable, "json"))
    # ## Testing the En-test output
    expect_true(inherits(output$ttest, "character"))
    # ## Testing the reactive list as output
    expect_true(inherits(r$compare03x, "reactivevalues"))
    expect_named(r$compare03x,
                c('click', 'data', 'ftest',
                  'normality', 'outliers', 'parameter',
                  'plotlyboxplot', 'summary', 'ttest',
                  'udm'),
                ignore.order = TRUE)
    expect_length(names(r$compare03x), 10)

    # switching the parameter
    r$compare03$myparameter <- "noeff"
    session$flushReact()

    # testing the inputs
    session$setInputs(udm = "ug/L")
    session$flushReact()
    expect_true(input$udm == "ug/L")

    # testing the intermediate dataset
    expect_equal(input_data()$response, c(2.14, 4.85))
    expect_equal(input_data()$uncertainty, c(1.02, 2.50))
})

test_that("module compare035 for 2 values input ui works", {
  ui <- mod_compare035_2values_unc_inputs_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare035_2values_unc_inputs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module compare035 for 2 values output ui works", {
  ui <- mod_compare035_2values_unc_output_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare035_2values_unc_output_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
