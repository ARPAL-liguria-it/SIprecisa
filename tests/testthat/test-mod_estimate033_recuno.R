############## TO BE COMPLETELY UPDATED #######################

r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    estimate03 = reactiveValues())

testServer(
  mod_estimate032_rip_server,
  # Add here your module params
  args = list(r), {

    testdata <- tomato_yields[fertilizer == "a", pounds, parameter]
    testdata$pounds_b <- tomato_yields[fertilizer == "b", pounds][1:5]
    colnames(testdata)[2] <- "pounds_a"
    testdata <- rbindlist(
      list(testdata,
           data.table("parameter" = rep("yield", 5),
                      "pounds_a" = c(22.3, 21.2, 19.8, 17.5, 16.1),
                      "pounds_b" = c(20.9, 22.5, 18.5, 20.3, 19.5))
           )
      )

    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds_a"
    r$loadfile02$secondresponsevar <- "pounds_b"
    r$loadfile02$data <- testdata
    r$estimate03$myparameter <- "yield"

    #session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    session$setInputs(significance = 0.95,
                      udm = "ug/L")
    session$flushReact()
    expect_true(input$significance == 0.95)
    expect_true(input$udm == "ug/L")

    # testing the intermediate dataset
    expect_equal(rownumber(), 10)
    expect_equal(key(), 1:10)
    expect_equal(is_outlier(), rep(FALSE, 10))
    expect_equal(dim(input_data()), c(10, 8))
    expect_equal(dim(selected_data()), c(10, 8))
    expect_equal(colnames(input_data()), c("key", "outlier", "measure1",
                                           "measure2", "response",
                                           "rel_response", "abs_rel_response",
                                           "abs_perc_response"))
    expect_equal(input_data()$response, testdata[, pounds_a - pounds_b])

    # testing data points removal
    ## removal of the 5th data point
    outlierflag <- rep(FALSE, 10)
    keys(5)
    outlierflag5 <- outlierflag
    outlierflag5[5] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag5)
    expect_equal(selected_data()$key, c(1:4, 6:10))

    ## removal of 3th and 5th points
    keys(c(3, 5))
    outlierflag35 <- outlierflag5
    outlierflag35[3] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag35)
    expect_equal(selected_data()$key, c(1:2, 4, 6:10))

    ## re-adding the 5th point
    keys(3)
    outlierflag3 <- outlierflag
    outlierflag3[3] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag3)
    expect_equal(selected_data()$key, c(1:2, 4:10))

    ## re-adding all the points
    keys(NA)
    session$flushReact()

    expect_equal(is_outlier(), outlierflag)

    # testing shapiro-wilk test intermediate results
    expect_equal(shapiro_html(),
      "I valori non sono compatibili con una distribuzione normale (W = 0.806, <i>p</i>-value = 0.0170)</br>")

    # testing grubbs test intermediate results
    expect_equal(outliers_html(),
      "-70.09 è un possibile valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")

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
    ## Testing the precision output
    expect_true(inherits(output$precision, "character"))
    ## Testing the reactive list as output
    expect_true(inherits(r$estimate03x, "reactivevalues"))
    expect_named(r$estimate03x,
                c('click', 'data',
                  'normality', 'outliers',
                  'parameter', 'plotlyboxplot',
                  'plotlyconfint', "precision",
                  'significance','summary', 'udm',
                  'trueness', 'ttest'),
                ignore.order = TRUE)
    expect_length(names(r$estimate03x), 13)
})

test_that("module estimate032 for precision parameters on paired values input ui works", {
  ui <- mod_estimate032_rip_inputs_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate032_rip_inputs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module estimate032 for precision parameters on paired values output ui works", {
  ui <- mod_estimate032_rip_output_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate032_rip_output_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
