r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    compare03 = reactiveValues())

testServer(
  mod_compare031_2samples_server,
  # Add here your module params
  args = list(r), {

    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$data <- tomato_yields
    r$compare03$myparameter <- "yield"
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    session$setInputs(alternative = "different",
                      significance = 0.95,
                      udm = "ug/L")
    session$flushReact()
    expect_true(input$alternative == "different")
    expect_true(input$significance == 0.95)
    expect_true(input$udm == "ug/L")

    # testing the intermediate dataset
    expect_equal(rownumber(), 11)
    expect_equal(key(), 1:11)
    expect_equal(is_outlier(), rep(FALSE, 11))
    expect_equal(dim(input_data()), c(11, 4))
    expect_equal(dim(selected_data()), c(11, 4))
    expect_equal(colnames(input_data()), c("key", "outlier", "response", "group"))
    expect_equal(input_data()$response, tomato_yields$pounds)
    expect_equal(input_data()$group, tomato_yields$fertilizer)

    # testing data points removal
    ## removal of the 5th data point
    outlierflag <- rep(FALSE, 11)
    keys(6)
    outlierflag6 <- outlierflag
    outlierflag6[6] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag6)
    expect_equal(selected_data()$key, c(1:5, 7:11))

    ## removal of 5th and 7th points
    keys(c(6, 7))
    outlierflag67 <- outlierflag6
    outlierflag67[7] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag67)
    expect_equal(selected_data()$key, c(1:5, 8:11))

    ## re-adding the 6th point
    keys(7)
    outlierflag7 <- outlierflag
    outlierflag7[7] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag7)
    expect_equal(selected_data()$key, c(1:6, 8:11))

    ## re-adding all the points
    keys(NA)
    session$flushReact()

    expect_equal(is_outlier(), rep(FALSE, 11))

    # testing shapiro-wilk test intermediate results
    expect_equal(lvl(), c("a", "b"))
    expect_equal(shapirotest_list()[[1]],
      "<b>Gruppo a:</b> I valori sono compatibili con una distribuzione normale (W = 0.990, <i>p</i>-value = 0.9803)</br>")
    expect_equal(shapirotest_list()[[2]],
      "<b>Gruppo b:</b> I valori sono compatibili con una distribuzione normale (W = 0.926, <i>p</i>-value = 0.5512)</br>")

    # testing grubbs test intermediate results
    expect_equal(outtest_list()[[1]],
      "<b>Gruppo a:</b></br> nessun valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")
    expect_equal(outtest_list()[[2]],
      "<b>Gruppo b:</b></br> nessun valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")

    # Testing the outputs
    ## Testing the boxplot output
    expect_true(inherits(output$boxplot, "json"))
    ## Testing the summary output
    expect_true(inherits(output$summarytable, "json"))
    ## Testing the Shapiro-Wilk test output
    expect_true(inherits(output$shapirotest, "character"))
    ## Testing the GESD test output
    expect_true(inherits(output$outliers, "character"))
    # ## Testing the t-test output
    expect_true(inherits(output$ttest, "character"))
    # ## Testing the F-test output
    expect_true(inherits(output$ftest, "character"))
    # ## Testing the reactive list as output
    expect_true(inherits(r$compare03x, "reactivevalues"))
    expect_named(r$compare03x,
                c('alternative', 'click', 'data',
                  'ftest', 'normality', 'outliers',
                  'parameter', 'plotlyboxplot', 'significance',
                  'summary', 'ttest', 'udm'),
                ignore.order = TRUE)
    expect_length(names(r$compare03x), 12)
})

test_that("module compare031 for 2 samples input ui works", {
  ui <- mod_compare031_2samples_inputs_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare031_2samples_inputs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module compare031 for 2 samples output ui works", {
  ui <- mod_compare031_2samples_output_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare031_2samples_output_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
