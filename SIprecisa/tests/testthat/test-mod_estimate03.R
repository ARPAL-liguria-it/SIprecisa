r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    estimate03x = reactiveValues())


# riprec option ----
testServer(
  mod_estimate03_server,
  # Add here your module params
  args = list(r), {

    # testing for riprec option
    r$aim01$aim <- "riprec"
    r$loadfile02$data <- tomato_yields[fertilizer == "b", .(parameter, pounds)]
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")

    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_equal(r$estimate03$myparameter, "yield")

    r$estimate03x$significance <- 0.95
    r$estimate03x$udm <- "ug/L"
    r$estimate03x$refvalue <- tomato_yields[fertilizer == "a", mean(pounds)]
    r$estimate03x$refuncertainty <- 0
    r$estimate03x$submit <- 1
    r$estimate03x$click <- 1

    session$flushReact()

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    session$flushReact()

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$estimate03$yield,
                 c('boxplot', 'data',
                   'normality', 'normality_html',
                   'outliers', 'outliers_html',
                   'parameter', 'refuncertainty', 'refvalue',
                   'plotlyboxplot', 'plotlyconfint',
                   'precision', 'precision_html',
                   'saved', 'significance',
                   'summary', 'udm',
                   'trueness', 'trueness_html',
                   'ttest', 'ttest_html'), ignore.order = TRUE)
    expect_length(names(r$estimate03$yield), 21)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })


# rip option ----
testdata <- tomato_yields[fertilizer == "a", pounds, parameter]
testdata$pounds_b <- tomato_yields[fertilizer == "b", pounds][1:5]
colnames(testdata)[2] <- "pounds_a"
testdata <- data.table::rbindlist(
  list(testdata,
       data.table::data.table("parameter" = factor(rep("yield", 5)),
                              "pounds_a" = c(22.3, 21.2, 19.8, 17.5, 16.1),
                              "pounds_b" = c(20.9, 22.5, 18.5, 20.3, 19.5))
  )
)

testServer(
  mod_estimate03_server,
  # Add here your module params
  args = list(r), {

    # testing for rip option
    r$aim01$aim <- "rip"
    r$loadfile02$data <- testdata
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds_a"
    r$loadfile02$secondresponsevar <- "pounds_b"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")

    r$estimate03x$significance <- 0.95
    r$estimate03x$udm <- "ug/L"
    r$estimate03x$click <- 1

    session$flushReact()

    ns <- session$ns

    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_equal(r$estimate03$myparameter, "yield")

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)
    expect_equal(savedel_flag(), "delete")

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$estimate03$yield,
                 c('boxplot', 'data', 'normality',
                   'normality_html', 'outliers',
                   'outliers_html', 'parameter',
                   'plotlyboxplot', 'plotlyconfint',
                   'precision', 'precision_html',
                   'saved', 'significance',
                   'summary', 'trueness',
                   'trueness_html', 'ttest',
                   'ttest_html', 'udm'),
                 ignore.order = TRUE)
    expect_length(names(r$estimate03$yield), 19)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })


# recuno option ----
yeseffectdata <- data.table::data.table(myparameter = "analyte",
                                        myvalue = 0.0239,
                                        myuncertainty = 0.0036)

refv <- 0.044
refu <- 0.0082

testServer(
  mod_estimate03_server,
  # Add here your module params
  args = list(r), {

    # testing for recuno option
    r$aim01$aim <- "recuno"
    r$loadfile02$data <- yeseffectdata
    r$loadfile02$parvar <- "myparameter"
    r$loadfile02$responsevar <- "myvalue"
    r$loadfile02$uncertaintyvar <- "myuncertainty"
    r$loadfile02$parlist <- "analyte"

    session$setInputs(parameter = "analyte")

    r$estimate03x$udm <- "ug/L"
    r$estimate03x$refvalue <- refv
    r$estimate03x$refuncertainty <- refu
    r$estimate03x$submit <- 1
    r$estimate03x$click <- 1

    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$estimate03$myparameter == "analyte")

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$estimate03$analyte,
                 c('boxplot', 'data',
                   'normality', 'normality_html',
                   'outliers', 'outliers_html',
                   'parameter', 'plotlyboxplot',
                   'plotlyconfint', 'precision',
                   'precision_html', 'refuncertainty',
                   'refvalue', 'saved', 'summary',
                   'trueness', 'trueness_html',
                   'ttest', 'ttest_html', 'udm'),
                 ignore.order = TRUE)
    expect_length(names(r$estimate03$analyte), 20)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })

test_that("module estimateinput ui works", {
  ui <- mod_estimate03_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate03_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

