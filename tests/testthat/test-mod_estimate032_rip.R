r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    estimate03 = reactiveValues())

testServer(
  mod_estimate032_rip_server,
  # Add here your module params
  args = list(r), {

    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds_a"
    r$loadfile02$data <- tomato_yields[fertilizer == "b", .(parameter, pounds)]
    r$estimate03$myparameter <- "yield"

    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    session$setInputs(significance = 0.95,
                      refvalue = tomato_yields[fertilizer == "a", mean(pounds)],
                      refuncertainty = 0,
                      udm = "ug/L",
                      submit = 1)
    session$flushReact()
    expect_true(input$significance == 0.95)
    expect_true(input$refvalue == 20.84)
    expect_true(input$refuncertainty == 0)
    expect_true(input$udm == "ug/L")
    expect_true(input$submit == 1)

    # testing the intermediate dataset
    expect_equal(rownumber(), 6)
    expect_equal(key(), 1:6)
    expect_equal(is_outlier(), rep(FALSE, 6))
    expect_equal(dim(input_data()), c(6, 3))
    expect_equal(dim(selected_data()), c(6, 3))
    expect_equal(colnames(input_data()), c("key", "outlier", "response"))
    expect_equal(input_data()$response, tomato_yields[fertilizer == "b", pounds])

    # testing data points removal
    ## removal of the 5th data point
    outlierflag <- rep(FALSE, 6)
    keys(6)
    outlierflag6 <- outlierflag
    outlierflag6[6] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag6)
    expect_equal(selected_data()$key, c(1:5))

    ## removal of 4th and 6th points
    keys(c(4, 6))
    outlierflag46 <- outlierflag6
    outlierflag46[4] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag46)
    expect_equal(selected_data()$key, c(1:3, 5))

    ## re-adding the 6th point
    keys(4)
    outlierflag4 <- outlierflag
    outlierflag4[4] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag4)
    expect_equal(selected_data()$key, c(1:3, 5:6))

    ## re-adding all the points
    keys(NA)
    session$flushReact()

    expect_equal(is_outlier(), outlierflag)

    # testing shapiro-wilk test intermediate results
    expect_equal(shapiro_html(),
      "I valori sono compatibili con una distribuzione normale (W = 0.926, <i>p</i>-value = 0.5512)</br>")

    # testing grubbs test intermediate results
    expect_equal(outliers_html(),
                 "nessun valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")

    # Testing the outputs
    ## Testing the boxplot output
    expect_true(inherits(output$boxplot, "json"))
    ## Testing the confint output
    expect_true(inherits(output$confint, "json"))
    ## Testing the summary output
    expect_true(inherits(output$summarytable, "json"))
    ## Testing the Shapiro-Wilk test output
    expect_true(inherits(output$shapirotest, "character"))
    ## Testing the GESD test output
    expect_true(inherits(output$outliers, "character"))
    ## Testing the t-test output
    expect_true(inherits(output$ttest, "character"))
    ## Testing the trueness output
    expect_true(inherits(output$trueness, "character"))
    ## Testing the precision output
    expect_true(inherits(output$precision, "character"))
    ## Testing the reactive list as output
    expect_true(inherits(r$estimate03x, "reactivevalues"))
    expect_named(r$estimate03x,
                c('click', 'data',
                  'normality', 'outliers',
                  'parameter', 'plotlyboxplot',
                  'plotlyconfint', "precision",
                  'refuncertainty', 'refvalue',
                  'significance', 'summary',
                  'trueness', 'ttest', 'udm'),
                ignore.order = TRUE)
    expect_length(names(r$estimate03x), 15)
})

test_that("module estimate031 for precision and trueness performance parameters input ui works", {
  ui <- mod_estimate031_riprec_inputs_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate031_riprec_inputs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module estimate031 for precision and trueness performance parameters output ui works", {
  ui <- mod_estimate031_riprec_output_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate031_riprec_output_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
