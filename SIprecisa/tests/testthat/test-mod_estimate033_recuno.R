############## TO BE COMPLETELY UPDATED #######################

r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    estimate03 = reactiveValues())

testServer(
  mod_estimate033_recuno_server,
  # Add here your module params
  args = list(r), {

    yeseffectdata <- data.table::data.table(myparameter = "analyte",
                                myvalue = 0.0239,
                                myuncertainty = 0.0036)

    refv <- 0.044
    refu <- 0.0082

    r$loadfile02$parvar <- "myparameter"
    r$loadfile02$responsevar <- "myvalue"
    r$loadfile02$uncertaintyvar <- "myuncertainty"
    r$loadfile02$data <- yeseffectdata
    r$estimate03$myparameter <- "analyte"

    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    # testing the inputs
    session$setInputs(refvalue = refv,
                      refuncertainty = refu,
                      udm = "ug/L",
                      submit = 1)
    session$flushReact()
    expect_true(input$refvalue == refv)
    expect_true(input$refuncertainty == refu)
    expect_true(input$udm == "ug/L")
    expect_true(input$submit == 1)
    expect_equal(r$estimate03$myparameter, "analyte")
    expect_equal(r$estimate03x$udm, "ug/L")
    expect_equal(r$estimate03x$click, 1)
    expect_equal(r$estimate03x$refuncertainty, refu)

    # testing the intermediate dataset
    expect_equal(ok_click(), 1)
    expect_equal(ok_calc(), 1)
    expect_equal(dim(mydata()), c(1, 3))
    expect_equal(dim(input_data()), c(1, 2))
    expect_equal(minval(), 1)
    expect_named(input_data(), c("response", "uncertainty"))
    expect_equal(input_data()$response, yeseffectdata$myvalue)

    # testing intermediate results
    expect_equal(entest_html(),
"<h4> Test per valutare la presenza di bias (E number) </h4>
<b>H0:</b> valore di riferimento = valore misurato </br>
<b>H1:</b> valore di riferimento \u2260 valore misurato
<ul>
  <li> Differenza tra i due valori (valore e intervallo di confidenza) = 0.02010 ug/L, 0.01114 \u2013 0.02906 ug/L</li>
  <li> E<sub>n</sub> sperimentale = 2.244 </li>
  <li> E<sub>n</sub> critico = 1.000 </li>
</ul>
\u21e8 valore di riferimento e valore misurato sono differenti")

    expect_equal(trueness_html(),
"<ul>
  <li> Recupero = 54.3 &percnt;</li>
  <li> Bias = -0.0201 ug/L</li>
  <li> Bias = -84.1 &percnt;</li>
</ul>")


    # Testing the outputs
    ## Testing the boxplot output
    expect_true(inherits(output$boxplot, "json"))
    ## Testing the summary output
    expect_true(inherits(output$summarytable, "json"))
    ## Testing the En-test output
    expect_true(inherits(output$ttest, "character"))
    ## Testing the reactive list as output
    expect_true(inherits(r$estimate03x, "reactivevalues"))
    expect_named(r$estimate03x,
                c('click', 'data',
                  'normality', 'outliers',
                  'parameter', 'plotlyboxplot',
                  'plotlyconfint', "precision",
                  'refuncertainty', 'refvalue',
                  'summary', 'udm',
                  'trueness', 'ttest'),
                ignore.order = TRUE)
    expect_length(names(r$estimate03x), 14)
})

test_that("module estimate033 for precision parameters on paired values input ui works", {
  ui <- mod_estimate033_recuno_inputs_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate033_recuno_inputs_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

test_that("module estimate033 for precision parameters on paired values output ui works", {
  ui <- mod_estimate033_recuno_output_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate033_recuno_output_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})
