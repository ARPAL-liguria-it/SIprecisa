values2unc <- data.table::fread(
  system.file("extdata", "raw_2values_unc.csv", package = "SIconfronta"),
  header = "auto",
  stringsAsFactors = TRUE)

sample1 <- data.table::fread(
  system.file("extdata", "raw_1sample.csv", package = "SIconfronta"),
  header = "auto",
  stringsAsFactors = TRUE)


r <- reactiveValues(aim01 = reactiveValues())

# testing for 2 samples
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "2samples"
             session$flushReact()

             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))

             # checking the upload process
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_tomato_yields.csv", package = "SIconfronta"),
               name = "raw_tomato_yields.csv"
             ))

             # filename
             expect_equal(input$file$name, "raw_tomato_yields.csv")
             # datapath
             expect_equal(
               input$file$datapath,
               system.file("extdata", "raw_tomato_yields.csv", package = "SIconfronta")
             )

             # column names
             expect_named(datafile(), c("parameter", "fertilizer", "pounds"))
             # dimensions of the dataset
             expect_equal(dim(datafile()), c(11, 3))
             # the first column is factor
             expect_equal(class(datafile()[["parameter"]]), "factor")
             # the second column is numeric
             expect_equal(class(datafile()[["fertilizer"]]), "factor")
             # the third column is numeric
             expect_equal(class(datafile()[["pounds"]]), "numeric")
             # number of values for the two groups
             expect_equal(datafile()[, .N, by = "fertilizer"][, N], c(5, 6))


             # required number of numeric columns
             expect_equal(reqsumnum(), 1)
             # required number of grouping levels
             expect_equal(reqsumgroup(), 2)
             # required max number of row for each parameter and group pair
             expect_equal(reqmaxvalues(), 30)
             # required min number of row for each parameter and group pair
             expect_equal(reqminvalues(), 5)
             # numeric columns
             expect_equal(numcol(), "pounds")
             # number of numeric columns
             expect_equal(sumnum(), 1)
             # sumnum is TRUE
             expect_equal(numok(), TRUE)
             # character columns
             expect_equal(charcol(), c("parameter", "fertilizer"))
             # number of factor columns
             expect_equal(sumchar(), 2)
             # charok is TRUE
             expect_equal(charok(), TRUE)
             # dataloaded flag
             expect_equal(dataloaded(), "dataloaded")

             session$setInputs(
               parvar = "parameter",
               groupvar = "fertilizer",
               responsevar = "pounds"
               )

             # maxgroup is equal to 2
             expect_equal(maxgroup(), 2)
             # mingroup is equal to 2
             expect_equal(mingroup(), 2)
             # groupok is TRUE
             expect_equal(groupok(), TRUE)
             # maxvalues is less or equal 30
             expect_true(maxvalues() <= 30)
             # minvalues is greater or equal 5
             expect_true(minvalues() >= 5)
             # valuesok is TRUE
             expect_equal(valuesok(), TRUE)
             # dataok is TRUE
             expect_equal(dataok(), TRUE)
             # isloaded is dataloaded
             expect_equal(isloaded(), "dataloaded")
             # is2values is not_2values
             expect_equal(is2values(), "not_2values")
             # parlist is yields
             expect_equal(parlist(), factor("yield"))

             session$setInputs(nextbtn = 1,
                               yesbtn = 1)

             # saved output
             expect_equal(class(r$loadfile02), "reactivevalues")
             expect_equal(r$loadfile02$data, datafile())
             expect_equal(r$loadfile02$parvar, "parameter")
             expect_equal(r$loadfile02$parlist, factor("yield"))
             expect_equal(r$loadfile02$groupvar, "fertilizer")
             expect_equal(r$loadfile02$responsevar, "pounds")
             expect_equal(r$loadfile02$uncertaintyvar, NULL)

           })

# testing for 1 sample
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "1sample_mu"
             session$flushReact()

             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))

             # checking the upload process
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample.csv", package = "SIconfronta"),
               name = "raw_1sample.csv"
             ))

             # filename
             expect_equal(input$file$name, "raw_1sample.csv")
             # datapath
             expect_equal(
               input$file$datapath,
               system.file("extdata", "raw_1sample.csv", package = "SIconfronta")
             )

             # column names
             expect_equal(colnames(datafile()),
                          c("analita", "gruppo", "valore"))
             # dimensions of the dataset
             expect_equal(dim(datafile()), c(15, 3))
             # the first column is factor
             expect_equal(class(datafile()[["analita"]]), "factor")
             # the second column is numeric
             expect_equal(class(datafile()[["gruppo"]]), "factor")
             # the third column is numeric
             expect_equal(class(datafile()[["valore"]]), "numeric")
             # number of values for the two groups
             expect_equal(datafile()[, .N, by = "gruppo"][, N], 15)


             # required number of numeric columns
             expect_equal(reqsumnum(), 1)
             # required number of grouping levels
             expect_equal(reqsumgroup(), 1)
             # numeric columns
             expect_equal(numcol(), "valore")
             # number of numeric columns
             expect_equal(sumnum(), 1)
             # sumnum is TRUE
             expect_equal(numok(), TRUE)
             # character columns
             expect_equal(charcol(), c("analita", "gruppo"))
             # number of factor columns
             expect_equal(sumchar(), 2)
             # charok is TRUE
             expect_equal(charok(), TRUE)
             # dataloaded flag
             expect_equal(dataloaded(), "dataloaded")

             session$setInputs(
               parvar = "analita",
               groupvar = "gruppo",
               responsevar = "valore"
             )

             # maxgroup is equal to 1
             expect_equal(maxgroup(), 1)
             # mingroup is equal to 1
             expect_equal(mingroup(), 1)
             # groupok is TRUE
             expect_equal(groupok(), TRUE)
             # maxvalues is less or equal 30
             expect_true(maxvalues() <= 30)
             # minvalues is greater or equal 5
             expect_true(minvalues() >= 5)
             # valuesok is TRUE
             expect_equal(valuesok(), TRUE)
             # dataok is TRUE
             expect_equal(dataok(), TRUE)
             # isloaded is dataloaded
             expect_equal(isloaded(), "dataloaded")
             # is2values is not_2values
             expect_equal(is2values(), "not_2values")
             # parlist is yields
             expect_equal(parlist(), factor(c("yield", "load")))

             session$setInputs(nextbtn = 1,
                               yesbtn = 1)

             # saved output
             expect_equal(class(r$loadfile02), "reactivevalues")
             expect_equal(r$loadfile02$data, datafile())
             expect_equal(r$loadfile02$parvar, "analita")
             expect_equal(r$loadfile02$parlist, factor(c("yield", "load")))
             expect_equal(r$loadfile02$groupvar, "gruppo")
             expect_equal(r$loadfile02$responsevar, "valore")
             expect_equal(r$loadfile02$uncertaintyvar, NULL)

           })

# testing for 2 values
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "2values_unc"
             session$flushReact()

             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))

             # checking the upload process
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc.csv", package = "SIconfronta"),
               name = "raw_2values_unc.csv"
             ))

             # filename
             expect_equal(input$file$name, "raw_2values_unc.csv")
             # datapath
             expect_equal(
               input$file$datapath,
               system.file("extdata", "raw_2values_unc.csv", package = "SIconfronta")
             )

             # column names
             expect_equal(colnames(datafile()),
                          c("analita", "gruppo", "valore", "unc"))
             # dimensions of the dataset
             expect_equal(dim(datafile()), c(4, 4))
             # the first column is factor
             expect_equal(class(datafile()[["analita"]]), "factor")
             # the second column is numeric
             expect_equal(class(datafile()[["gruppo"]]), "factor")
             # the third column is numeric
             expect_equal(class(datafile()[["valore"]]), "numeric")
             # number of values for the two groups
             expect_equal(datafile()[, .N, by = "gruppo"][, N], c(2, 2))


             # required number of numeric columns
             expect_equal(reqsumnum(), 2)
             # required number of grouping levels
             expect_equal(reqsumgroup(), 2)
             # required max number of row for each parameter and group pair
             expect_equal(reqmaxvalues(), 1)
             # required min number of row for each parameter and group pair
             expect_equal(reqminvalues(), 1)
             # numeric columns
             expect_equal(numcol(), c("valore", "unc"))
             # number of numeric columns
             expect_equal(sumnum(), 2)
             # sumnum is TRUE
             expect_equal(numok(), TRUE)
             # character columns
             expect_equal(charcol(), c("analita", "gruppo"))
             # number of factor columns
             expect_equal(sumchar(), 2)
             # charok is TRUE
             expect_equal(charok(), TRUE)
             # dataloaded flag
             expect_equal(dataloaded(), "dataloaded")

             session$setInputs(
               parvar = "analita",
               groupvar = "gruppo",
               responsevar = "valore",
               uncertaintyvar = "unc"
             )

             # maxgroup is equal to 2
             expect_equal(maxgroup(), 2)
             # mingroup is equal to 2
             expect_equal(mingroup(), 2)
             # groupok is TRUE
             expect_equal(groupok(), TRUE)
             # maxvalues is less or equal 1
             expect_true(maxvalues() <= 1)
             # minvalues is greater or equal 1
             expect_true(minvalues() >= 1)
             # valuesok is TRUE
             expect_equal(valuesok(), TRUE)
             # dataok is TRUE
             expect_equal(dataok(), TRUE)
             # isloaded is dataloaded
             expect_equal(isloaded(), "dataloaded")
             # is2values is 2values
             expect_equal(is2values(), "2values")
             # parlist is yields
             expect_equal(parlist(), factor(c("L03", "L12")))

             session$setInputs(nextbtn = 1,
                               yesbtn = 1)

             # saved output
             expect_equal(class(r$loadfile02), "reactivevalues")
             expect_equal(r$loadfile02$data, datafile())
             expect_equal(r$loadfile02$parvar, "analita")
             expect_equal(r$loadfile02$parlist, factor(c("L03", "L12")))
             expect_equal(r$loadfile02$groupvar, "gruppo")
             expect_equal(r$loadfile02$responsevar, "valore")
             expect_equal(r$loadfile02$uncertaintyvar, "unc")

           })

# testing errors for 2samples and wrong number of groups, columns and rows
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "2samples"
             session$flushReact()

             ns <- session$ns

             # 1 group instead of 2
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample.csv", package = "SIconfronta"),
               name = "raw_1sample.csv"
             ))

             expect_error(groupok())

             # 3 groups on some parameters instead of 2 on all parameters
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2samples_wronggroups.csv", package = "SIconfronta"),
               name = "raw_2samples_wronggroups.csv"
             ))

             expect_error(groupok())

             # no grouping variable
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_nogroups.csv", package = "SIconfronta"),
               name = "raw_nogroups.csv"
             ))

             expect_error(charok())
             expect_error(groupok())

             # 2 numerical variable instead of 1
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc.csv", package = "SIconfronta"),
               name = "raw_2values_unc.csv"
             ))

             expect_error(numok())

             # 5 values for a parameter and group pair insted of a minimum of 6 values
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2samples_4rows.csv", package = "SIconfronta"),
               name = "raw_2samples_4rows.csv"
             ))

             expect_error(valuesok())

           })

# testing errors for 1sample  and wrong number of groups or columns
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "1sample_sigma"
             session$flushReact()

             ns <- session$ns

             # 2 groups instead of 1
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2samples.csv", package = "SIconfronta"),
               name = "raw_2samples.csv"
             ))

             expect_error(groupok())

             # 3 groups on some parameters instead of 1 on all parameters
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample_wronggroups.csv", package = "SIconfronta"),
               name = "raw_1sample_wronggroups.csv"
             ))

             expect_error(groupok())

             # no grouping variable
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_nogroups.csv", package = "SIconfronta"),
               name = "raw_nogroups.csv"
             ))

             expect_error(charok())
             expect_error(groupok())

             # 2 numerical variable instead of 1
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc.csv", package = "SIconfronta"),
               name = "raw_2values_unc.csv"
             ))

             expect_error(numok())
             expect_error(groupok())

             # 5 values for a parameter and group pair insted of a minimum of 6 values
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample_4rows.csv", package = "SIconfronta"),
               name = "raw_1sample_4rows.csv"
             ))

             expect_error(valuesok())

           })

# testing errors for 2values_unc and wrong number of groups or columns
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "2values_unc"
             session$flushReact()

             ns <- session$ns

             # 1 numerical variable instead of 2
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2samples.csv", package = "SIconfronta"),
               name = "raw_2samples.csv"
             ))

             expect_error(numok())

             # 3 groups on some parameters instead of 1 on all parameters
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc_wronggroups.csv", package = "SIconfronta"),
               name = "raw_2values_unc_wronggroups.csv"
             ))

             expect_error(groupok())

             # no grouping variable
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_nogroups.csv", package = "SIconfronta"),
               name = "raw_nogroups.csv"
             ))

             expect_error(numok())
             expect_error(charok())
             expect_error(groupok())

             # 2 values for a parameter and group pair insted of a minimum of 1 value
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample_4rows.csv", package = "SIconfronta"),
               name = "raw_1sample_4rows.csv"
             ))

             expect_error(valuesok())

           })

test_that("module ui works", {
  ui <- mod_loadfile02_ui(id = "test")
  golem::expect_shinytaglist(ui)
  # Check that formals have not been removed
  fmls <- formals(mod_loadfile02_ui)
  for (i in c("id")) {
    expect_true(i %in% names(fmls))
  }
})

