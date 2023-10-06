r <- reactiveValues(aim01 = reactiveValues(),
                    loadfile02 = reactiveValues(),
                    compare03x = reactiveValues())


# 2 samples with values
testServer(
  mod_compare03_server,
  # Add here your module params
  args = list(r), {

    # testing for 2samples
    r$aim01$aim <- "2samples"
    r$loadfile02$data <- tomato_yields
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$compare03$myparameter == "yield")

    r$compare03x$alternative <- "different"
    r$compare03x$significance <- "0.95"
    r$compare03x$udm <- "ug/L"
    session$flushReact()

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$compare03$yield,
                 c('alternative', 'boxplot', 'data',
                   'ftest', 'ftest_html', 'normality',
                   'normality_html', 'outliers', 'outliers_html',
                   'parameter', 'plotlyboxplot', 'saved',
                   'significance', 'summary', 'ttest',
                   'ttest_html', 'udm'), ignore.order = TRUE)
    expect_length(names(r$compare03$yield), 17)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })


# 2 samples with parameters
tomato_a <- tomato_yields[fertilizer == "a"]
tomato_a$fertilizer <- tomato_a$fertilizer |> droplevels()
label2 <- "b"
tomato_b <- tomato_yields[fertilizer == "b"]
tomato_b$fertilizer <- tomato_b$fertilizer |> droplevels()
mean2 <- tomato_b[, mean(pounds)]
sd2 <- tomato_b[, sd(pounds)]
n2 <- tomato_b[, .N]

testServer(
  mod_compare03_server,
  # Add here your module params
  args = list(r), {

    # testing for 2samples with parameters
    r$aim01$aim <- "2samples_par"
    r$loadfile02$data <- tomato_a
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$compare03$myparameter == "yield")

    r$compare03x$alternative <- "different"
    r$compare03x$significance <- "0.95"
    r$compare03x$udm <- "ug/L"
    r$compare03x$label2 <- label2
    r$compare03x$mean2 <- mean2
    r$compare03x$sd2 <- sd2
    r$compare03x$n2 <- n2
    r$compare03x$click <- 1
    session$flushReact()

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$compare03$yield,
                 c('alternative', 'boxplot', 'data',
                   'ftest', 'ftest_html', 'label2',
                   'mean2', 'n2', 'normality',
                   'normality_html', 'outliers', 'outliers_html',
                   'parameter', 'plotlyboxplot', 'saved',
                   'sd2', 'significance', 'summary',
                   'ttest', 'ttest_html', 'udm'), ignore.order = TRUE)
    expect_length(names(r$compare03$yield), 21)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })


# 1 sample with known reference mean
tomato_a <- tomato_yields[fertilizer == "a"]
tomato_a$fertilizer <- tomato_a$fertilizer |> droplevels()
label2 <- "b"
tomato_b <- tomato_yields[fertilizer == "b"]
tomato_b$fertilizer <- tomato_b$fertilizer |> droplevels()
mean2 <- tomato_b[, mean(pounds)]
sd2 <- tomato_b[, sd(pounds)]
n2 <- tomato_b[, .N]

testServer(
  mod_compare03_server,
  # Add here your module params
  args = list(r), {

    # testing for 2samples with parameters
    r$aim01$aim <- "1sample_mu"
    r$loadfile02$data <- tomato_a
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$compare03$myparameter == "yield")

    r$compare03x$alternative <- "different"
    r$compare03x$significance <- "0.95"
    r$compare03x$udm <- "ug/L"
    r$compare03x$label2 <- label2
    r$compare03x$mean2 <- mean2
    r$compare03x$click <- 1
    session$flushReact()

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$compare03$yield,
                 c('alternative', 'boxplot', 'data',
                   'ftest', 'ftest_html', 'label2',
                   'mean2', 'normality', 'normality_html',
                   'outliers', 'outliers_html', 'parameter',
                   'plotlyboxplot', 'saved', 'significance',
                   'summary', 'ttest', 'ttest_html', 'udm'),
                 ignore.order = TRUE)
    expect_length(names(r$compare03$yield), 19)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })

# 1 sample with known reference standard deviation
tomato_a <- tomato_yields[fertilizer == "a"]
tomato_a$fertilizer <- tomato_a$fertilizer |> droplevels()
label2 <- "b"
tomato_b <- tomato_yields[fertilizer == "b"]
tomato_b$fertilizer <- tomato_b$fertilizer |> droplevels()
mean2 <- tomato_b[, mean(pounds)]
sd2 <- tomato_b[, sd(pounds)]
n2 <- tomato_b[, .N]

testServer(
  mod_compare03_server,
  # Add here your module params
  args = list(r), {

    # testing for 1sample vs known population standard deviation
    r$aim01$aim <- "1sample_sigma"
    r$loadfile02$data <- tomato_a
    r$loadfile02$parvar <- "parameter"
    r$loadfile02$responsevar <- "pounds"
    r$loadfile02$groupvar <- "fertilizer"
    r$loadfile02$parlist <- "yield"

    session$setInputs(parameter = "yield")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$compare03$myparameter == "yield")

    r$compare03x$alternative <- "different"
    r$compare03x$significance <- "0.95"
    r$compare03x$udm <- "ug/L"
    r$compare03x$label2 <- label2
    r$compare03x$sd2 <- sd2
    r$compare03x$click <- 1
    session$flushReact()

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$compare03$yield,
                 c('alternative', 'boxplot', 'data',
                   'ftest', 'ftest_html', 'label2',
                   'normality', 'normality_html', 'outliers',
                   'outliers_html', 'parameter', 'plotlyboxplot',
                   'saved', 'sd2', 'significance', 'summary',
                   'ttest', 'ttest_html', 'udm'),
                 ignore.order = TRUE)
    expect_length(names(r$compare03$yield), 19)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })

effectdata <- data.table::data.table(
  myparameter = c("noeff", "noeff", "yeseff", "yeseff"),
  mygroup = rep(letters[1:2], 2),
  myvalue = c(2.14, 4.85, 4.84, 2.15),
  myuncertainty = c(1.02, 2.50, 2.30, 1.02))

testServer(
  mod_compare03_server,
  # Add here your module params
  args = list(r), {

    # testing for 2samples with parameters
    r$aim01$aim <- "2values_unc"
    r$loadfile02$data <- effectdata
    r$loadfile02$parvar <- "myparameter"
    r$loadfile02$responsevar <- "myvalue"
    r$loadfile02$uncertaintyvar <- "myuncertainty"
    r$loadfile02$groupvar <- "mygroup"
    r$loadfile02$parlist <- c("noeff", "yeseff")

    session$setInputs(parameter = "noeff")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$compare03$myparameter == "noeff")


    r$compare03x$udm <- "ug/L"
    session$flushReact()

    ## testing the flags
    expect_equal(savedel_flag(), "save")
    expect_equal(somethingsaved(), "not_saved")

    session$setInputs(save = 1)

    expect_true(input$save == 1)

    ## testing the outputs
    expect_named(r$compare03$noeff,
                 c('boxplot', 'data', 'ftest',
                   'ftest_html', 'normality', 'normality_html',
                   'outliers', 'outliers_html', 'parameter',
                   'plotlyboxplot', 'saved', 'summary',
                   'ttest', 'ttest_html', 'udm'), ignore.order = TRUE)
    expect_length(names(r$compare03$noeff), 15)

    ## testing the flags
    expect_equal(savedel_flag(), "delete")
    expect_equal(somethingsaved(), "saved")


  })


test_that("module compareinput ui works", {
  ui <- mod_compare03_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_compare03_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

