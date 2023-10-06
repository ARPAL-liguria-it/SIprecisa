r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    compare03 = reactiveValues())
label2 <- "b"
tomato_b <- tomato_yields[fertilizer == "b"]
tomato_a <-tomato_yields[fertilizer == "a"]
mean2 <- tomato_b[, mean(pounds)]
sd2 <- tomato_b[, sd(pounds)]
n2 <- tomato_b[, .N]
tomato_b$fertilizer <- tomato_b$fertilizer |> droplevels()
tomato_a$fertilizer <- tomato_a$fertilizer |> droplevels()

testServer(
  mod_compare033_1sample_mu_server,
  # Add here your module params
  args = list(r), {

    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$data <- tomato_a
    r$compare03$myparameter <- "yield"
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    session$setInputs(alternative = "different",
                      significance = 0.95,
                      udm = "ug/L",
                      label = label2,
                      mean = mean2,
                      submit = 1
                      )
    session$flushReact()
    expect_true(input$alternative == "different")
    expect_true(input$significance == 0.95)
    expect_true(input$udm == "ug/L")
    expect_true(input$label == label2)
    expect_true(input$mean == mean2)
    expect_true(input$submit == 1)

    # testing the intermediate dataset
    expect_equal(rownumber(), 5)
    expect_equal(key(), 1:5)
    expect_equal(is_outlier(), rep(FALSE, 5))
    expect_equal(dim(input_data()), c(5, 4))
    expect_equal(dim(selected_data()), c(5, 4))
    expect_equal(colnames(input_data()), c("key", "outlier", "response", "group"))
    expect_equal(input_data()$response, tomato_a[, pounds])
    expect_equal(input_data()$group, tomato_a[, fertilizer])

    # testing data points removal
    ## removal of the 2th data point
    outlierflag <- rep(FALSE, 5)
    keys(2)
    outlierflag2 <- outlierflag
    outlierflag2[2] <- TRUE
    session$flushReact()

    expect_equal(is_outlier(), outlierflag2)
    expect_equal(selected_data()$key, c(1, 3:5))

    ## re-adding all the points
    keys(NA)
    session$flushReact()

    expect_equal(is_outlier(), rep(FALSE, 5))

    # testing shapiro-wilk test intermediate results
    expect_equal(lvl(), c("a"))
    expect_equal(shapirotest_list()[[1]],
      "<b>Gruppo a:</b> I valori sono compatibili con una distribuzione normale (W = 0.990, <i>p</i>-value = 0.9803)</br>")

    # testing grubbs test intermediate results
    expect_equal(outtest_list()[[1]],
      "<b>Gruppo a:</b></br> nessun valore anomalo a un livello di confidenza del 95% </br> nessun valore anomalo a un livello di confidenza del 99% </br></br>")

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
    # ## Testing the reactive list as output
    expect_true(inherits(r$compare03x, "reactivevalues"))
    expect_named(r$compare03x,
                c('alternative', 'click', 'data',
                  'ftest', 'label2', 'mean2',
                  'normality', 'outliers', 'parameter',
                  'plotlyboxplot', 'significance', 'summary',
                  'ttest', 'udm'),
                ignore.order = TRUE)
    expect_length(names(r$compare03x), 14)
})

test_that("module compare033 for 1 sample input ui works", {
  ui <- mod_compare033_1sample_mu_inputs_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare033_1sample_mu_inputs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module compare033 for 1 samples output ui works", {
  ui <- mod_compare033_1sample_mu_output_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare033_1sample_mu_output_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
