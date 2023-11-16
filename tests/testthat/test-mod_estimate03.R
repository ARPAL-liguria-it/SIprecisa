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

    session$setInputs(parameter = "yield")
    session$flushReact()

    ns <- session$ns
    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    ## testing the inputs
    expect_true(r$estimate03$myparameter == "yield")

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
       data.table::data.table("parameter" = rep("yield", 5),
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

    session$setInputs(parameter = "yield")

    r$estimate03x$significance <- 0.95
    r$estimate03x$udm <- "ug/L"
    r$estimate03x$click <- 1

    ns <- session$ns

    expect_true(inherits(ns, "function"))
    expect_true(grepl(id, ns("")))
    expect_true(grepl("test", ns("test")))

    session$flushReact()
    ## testing the inputs
    expect_true(r$estimate03$myparameter == "yield")


    ## testing the flags
    # expect_equal(savedel_flag(), "save")
    # expect_equal(somethingsaved(), "not_saved")
    #
    # session$setInputs(save = 1)
    #
    # expect_true(input$save == 1)

    ## testing the outputs
    # expect_named(r$estimate03$yield,
    #              c('alternative', 'boxplot', 'data',
    #                'ftest', 'ftest_html', 'label2',
    #                'mean2', 'n2', 'normality',
    #                'normality_html', 'outliers', 'outliers_html',
    #                'parameter', 'plotlyboxplot', 'saved',
    #                'sd2', 'significance', 'summary',
    #                'ttest', 'ttest_html', 'udm'), ignore.order = TRUE)
    # expect_length(names(r$estimate03$yield), 21)
    #
    # ## testing the flags
    # expect_equal(savedel_flag(), "delete")
    # expect_equal(somethingsaved(), "saved")


  })


# 1 sample with known reference mean
# tomato_a <- tomato_yields[fertilizer == "a"]
# tomato_a$fertilizer <- tomato_a$fertilizer |> droplevels()
# label2 <- "b"
# tomato_b <- tomato_yields[fertilizer == "b"]
# tomato_b$fertilizer <- tomato_b$fertilizer |> droplevels()
# mean2 <- tomato_b[, mean(pounds)]
# sd2 <- tomato_b[, sd(pounds)]
# n2 <- tomato_b[, .N]
#
# testServer(
#   mod_estimate03_server,
#   # Add here your module params
#   args = list(r), {
#
#     # testing for 2samples with parameters
#     r$aim01$aim <- "1sample_mu"
#     r$loadfile02$data <- tomato_a
#     r$loadfile02$parvar <- "parameter"
#     r$loadfile02$responsevar <- "pounds"
#     r$loadfile02$groupvar <- "fertilizer"
#     r$loadfile02$parlist <- "yield"
#
#     session$setInputs(parameter = "yield")
#     session$flushReact()
#
#     ns <- session$ns
#     expect_true(inherits(ns, "function"))
#     expect_true(grepl(id, ns("")))
#     expect_true(grepl("test", ns("test")))
#
#     ## testing the inputs
#     expect_true(r$estimate03$myparameter == "yield")
#
#     r$estimate03x$alternative <- "different"
#     r$estimate03x$significance <- "0.95"
#     r$estimate03x$udm <- "ug/L"
#     r$estimate03x$label2 <- label2
#     r$estimate03x$mean2 <- mean2
#     r$estimate03x$click <- 1
#     session$flushReact()
#
#     ## testing the flags
#     expect_equal(savedel_flag(), "save")
#     expect_equal(somethingsaved(), "not_saved")
#
#     session$setInputs(save = 1)
#
#     expect_true(input$save == 1)
#
#     ## testing the outputs
#     expect_named(r$estimate03$yield,
#                  c('alternative', 'boxplot', 'data',
#                    'ftest', 'ftest_html', 'label2',
#                    'mean2', 'normality', 'normality_html',
#                    'outliers', 'outliers_html', 'parameter',
#                    'plotlyboxplot', 'saved', 'significance',
#                    'summary', 'ttest', 'ttest_html', 'udm'),
#                  ignore.order = TRUE)
#     expect_length(names(r$estimate03$yield), 19)
#
#     ## testing the flags
#     expect_equal(savedel_flag(), "delete")
#     expect_equal(somethingsaved(), "saved")
#
#
#   })
#
# # 1 sample with known reference standard deviation
# tomato_a <- tomato_yields[fertilizer == "a"]
# tomato_a$fertilizer <- tomato_a$fertilizer |> droplevels()
# label2 <- "b"
# tomato_b <- tomato_yields[fertilizer == "b"]
# tomato_b$fertilizer <- tomato_b$fertilizer |> droplevels()
# mean2 <- tomato_b[, mean(pounds)]
# sd2 <- tomato_b[, sd(pounds)]
# n2 <- tomato_b[, .N]
#
# testServer(
#   mod_estimate03_server,
#   # Add here your module params
#   args = list(r), {
#
#     # testing for 1sample vs known population standard deviation
#     r$aim01$aim <- "1sample_sigma"
#     r$loadfile02$data <- tomato_a
#     r$loadfile02$parvar <- "parameter"
#     r$loadfile02$responsevar <- "pounds"
#     r$loadfile02$groupvar <- "fertilizer"
#     r$loadfile02$parlist <- "yield"
#
#     session$setInputs(parameter = "yield")
#     session$flushReact()
#
#     ns <- session$ns
#     expect_true(inherits(ns, "function"))
#     expect_true(grepl(id, ns("")))
#     expect_true(grepl("test", ns("test")))
#
#     ## testing the inputs
#     expect_true(r$estimate03$myparameter == "yield")
#
#     r$estimate03x$alternative <- "different"
#     r$estimate03x$significance <- "0.95"
#     r$estimate03x$udm <- "ug/L"
#     r$estimate03x$label2 <- label2
#     r$estimate03x$sd2 <- sd2
#     r$estimate03x$click <- 1
#     session$flushReact()
#
#     ## testing the flags
#     expect_equal(savedel_flag(), "save")
#     expect_equal(somethingsaved(), "not_saved")
#
#     session$setInputs(save = 1)
#
#     expect_true(input$save == 1)
#
#     ## testing the outputs
#     expect_named(r$estimate03$yield,
#                  c('alternative', 'boxplot', 'data',
#                    'ftest', 'ftest_html', 'label2',
#                    'normality', 'normality_html', 'outliers',
#                    'outliers_html', 'parameter', 'plotlyboxplot',
#                    'saved', 'sd2', 'significance', 'summary',
#                    'ttest', 'ttest_html', 'udm'),
#                  ignore.order = TRUE)
#     expect_length(names(r$estimate03$yield), 19)
#
#     ## testing the flags
#     expect_equal(savedel_flag(), "delete")
#     expect_equal(somethingsaved(), "saved")
#
#
#   })
#
# effectdata <- data.table::data.table(
#   myparameter = c("noeff", "noeff", "yeseff", "yeseff"),
#   mygroup = rep(letters[1:2], 2),
#   myvalue = c(2.14, 4.85, 4.84, 2.15),
#   myuncertainty = c(1.02, 2.50, 2.30, 1.02))
#
# testServer(
#   mod_estimate03_server,
#   # Add here your module params
#   args = list(r), {
#
#     # testing for 2samples with parameters
#     r$aim01$aim <- "2values_unc"
#     r$loadfile02$data <- effectdata
#     r$loadfile02$parvar <- "myparameter"
#     r$loadfile02$responsevar <- "myvalue"
#     r$loadfile02$uncertaintyvar <- "myuncertainty"
#     r$loadfile02$groupvar <- "mygroup"
#     r$loadfile02$parlist <- c("noeff", "yeseff")
#
#     session$setInputs(parameter = "noeff")
#     session$flushReact()
#
#     ns <- session$ns
#     expect_true(inherits(ns, "function"))
#     expect_true(grepl(id, ns("")))
#     expect_true(grepl("test", ns("test")))
#
#     ## testing the inputs
#     expect_true(r$estimate03$myparameter == "noeff")
#
#
#     r$estimate03x$udm <- "ug/L"
#     session$flushReact()
#
#     ## testing the flags
#     expect_equal(savedel_flag(), "save")
#     expect_equal(somethingsaved(), "not_saved")
#
#     session$setInputs(save = 1)
#
#     expect_true(input$save == 1)
#
#     ## testing the outputs
#     expect_named(r$estimate03$noeff,
#                  c('boxplot', 'data', 'ftest',
#                    'ftest_html', 'normality', 'normality_html',
#                    'outliers', 'outliers_html', 'parameter',
#                    'plotlyboxplot', 'saved', 'summary',
#                    'ttest', 'ttest_html', 'udm'), ignore.order = TRUE)
#     expect_length(names(r$estimate03$noeff), 15)
#
#     ## testing the flags
#     expect_equal(savedel_flag(), "delete")
#     expect_equal(somethingsaved(), "saved")
#
#
#   })


test_that("module estimateinput ui works", {
  ui <- mod_estimate03_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_estimate03_ui)
  for (i in c("id")){
    expect_true(i %in% names(fmls))
  }
})

