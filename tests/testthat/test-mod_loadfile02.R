sample_riprec <- data.table::fread(
  system.file("extdata", "raw_sample_riprec.csv", package = "SIprecisa"),
  header = "auto",
  stringsAsFactors = TRUE)

sample_rip <- data.table::fread(
  system.file("extdata", "raw_sample_rip.csv", package = "SIprecisa"),
  header = "auto",
  stringsAsFactors = TRUE)

sample_recuno <- data.table::fread(
  system.file("extdata", "raw_sample_recuno.csv", package = "SIprecisa"),
  header = "auto",
  stringsAsFactors = TRUE)

r <- reactiveValues(aim01 = reactiveValues())

# testing for repeatability and recovery
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "riprec"
             session$flushReact()

             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))

             # checking the upload process
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_sample_riprec.csv", package = "SIprecisa"),
               name = "raw_sample_riprec.csv"
             ))

             # filename
             expect_equal(input$file$name, "raw_sample_riprec.csv")
             # datapath
             expect_equal(
               input$file$datapath,
               system.file("extdata", "raw_sample_riprec.csv", package = "SIprecisa")
             )

             # column names
             expect_named(datafile(), c("analita", "misura1"))
             # dimensions of the dataset
             expect_equal(dim(datafile()), c(38, 2))
             # the first column is factor
             expect_equal(class(datafile()[["analita"]]), "factor")
             # the second column is numeric
             expect_equal(class(datafile()[["misura1"]]), "numeric")
             # number of values for the two groups
             expect_equal(datafile()[, .N, by = "analita"][, N], c(19, 19))


             # required number of numeric columns
             expect_equal(reqsumnum(), 1)
             # required max number of row for each parameter and group pair
             expect_equal(reqmaxvalues(), 30)
             # required min number of row for each parameter and group pair
             expect_equal(reqminvalues(), 6)
             # numeric columns
             expect_equal(numcol(), "misura1")
             # number of numeric columns
             expect_equal(sumnum(), 1)
             # sumnum is TRUE
             expect_equal(numok(), TRUE)
             # character columns
             expect_equal(charcol(), "analita")
             # number of factor columns
             expect_equal(sumchar(), 1)
             # charok is TRUE
             expect_equal(charok(), TRUE)
             # dataloaded flag
             expect_equal(dataloaded(), "dataloaded")

             session$setInputs(
               parvar = "analita",
               responsevar = "misura1"
               )

             # maxvalues is less or equal 30
             expect_true(maxvalues() <= 30)
             # minvalues is greater or equal 5
             expect_true(minvalues() >= 6)
             # valuesok is TRUE
             expect_equal(valuesok(), TRUE)
             # dataok is TRUE
             expect_equal(dataok(), TRUE)
             # isloaded is dataloaded
             expect_equal(isloaded(), "dataloaded")
             # isunc is without_unc
             expect_equal(isunc(), "without_unc")
             # is2measures is not_2measures
             expect_equal(is2measures(), "not_2measures")
             # parlist is right
             expect_equal(parlist(), factor(c("boro", "ferro")))

             session$setInputs(nextbtn = 1,
                               yesbtn = 1)

             # saved output
             expect_equal(class(r$loadfile02), "reactivevalues")
             expect_equal(r$loadfile02$data, datafile())
             expect_equal(r$loadfile02$parvar, "analita")
             expect_equal(r$loadfile02$parlist, factor(c("boro", "ferro")))
             expect_equal(r$loadfile02$responsevar, "misura1")
             expect_equal(r$loadfile02$secondresponsevar, NULL)
             expect_equal(r$loadfile02$uncertaintyvar, NULL)

           })

# testing for repeatability
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "rip"
             session$flushReact()

             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))

             # checking the upload process
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_sample_rip.csv", package = "SIprecisa"),
               name = "raw_sample_rip.csv"
             ))

             # filename
             expect_equal(input$file$name, "raw_sample_rip.csv")
             # datapath
             expect_equal(
               input$file$datapath,
               system.file("extdata", "raw_sample_rip.csv", package = "SIprecisa")
             )

             # column names
             expect_equal(colnames(datafile()),
                          c("analita", "misura1", "misura2"))
             # dimensions of the dataset
             expect_equal(dim(datafile()), c(38, 3))
             # the first column is factor
             expect_equal(class(datafile()[["analita"]]), "factor")
             # the second column is numeric
             expect_equal(class(datafile()[["misura1"]]), "numeric")
             # the third column is numeric
             expect_equal(class(datafile()[["misura2"]]), "numeric")
             # number of values for the two analytes
             expect_equal(datafile()[, .N, by = "analita"][, N], c(19, 19))


             # required number of numeric columns
             expect_equal(reqsumnum(), 2)
             # required number of grouping levels
             # numeric columns
             expect_equal(numcol(), c("misura1", "misura2"))
             # number of numeric columns
             expect_equal(sumnum(), 2)
             # sumnum is TRUE
             expect_equal(numok(), TRUE)
             # character columns
             expect_equal(charcol(), "analita")
             # number of factor columns
             expect_equal(sumchar(), 1)
             # charok is TRUE
             expect_equal(charok(), TRUE)
             # dataloaded flag
             expect_equal(dataloaded(), "dataloaded")

             session$setInputs(
               parvar = "analita",
               responsevar = "misura1",
               secondresponsevar = "misura2"
             )

             # maxvalues is less or equal 30
             expect_true(maxvalues() <= 30)
             # minvalues is greater or equal 5
             expect_true(minvalues() >= 5)
             # valuesok is TRUE
             expect_equal(valuesok(), TRUE)
             # dataok is TRUE
             expect_equal(dataok(), TRUE)
             # isunc is without_unc
             expect_equal(isunc(), "without_unc")
             # is2measures is 2measures
             expect_equal(is2measures(), "2measures")
             # parlist is right
             expect_equal(parlist(), factor(c("boro", "ferro")))

             session$setInputs(nextbtn = 1,
                               yesbtn = 1)

             # saved output
             expect_equal(class(r$loadfile02), "reactivevalues")
             expect_equal(r$loadfile02$data, datafile())
             expect_equal(r$loadfile02$parvar, "analita")
             expect_equal(r$loadfile02$parlist, factor(c("boro", "ferro")))
             expect_equal(r$loadfile02$responsevar, "misura1")
             expect_equal(r$loadfile02$secondresponsevar, "misura2")
             expect_equal(r$loadfile02$uncertaintyvar, NULL)

           })

# testing for recovery on single value
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "recuno"
             session$flushReact()

             ns <- session$ns
             expect_true(inherits(ns, "function"))
             expect_true(grepl(id, ns("")))
             expect_true(grepl("test", ns("test")))

             # checking the upload process
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_sample_recuno.csv", package = "SIprecisa"),
               name = "raw_sample_recuno.csv"
             ))

             # filename
             expect_equal(input$file$name, "raw_sample_recuno.csv")
             # datapath
             expect_equal(
               input$file$datapath,
               system.file("extdata", "raw_sample_recuno.csv", package = "SIprecisa")
             )

             # column names
             expect_equal(colnames(datafile()),
                          c("analita", "misura", "extunc"))
             # dimensions of the dataset
             expect_equal(dim(datafile()), c(2, 3))
             # the first column is factor
             expect_equal(class(datafile()[["analita"]]), "factor")
             # the second column is numeric
             expect_equal(class(datafile()[["misura"]]), "numeric")
             # the third column is numeric
             expect_equal(class(datafile()[["extunc"]]), "numeric")

             # required number of numeric columns
             expect_equal(reqsumnum(), 2)
             # required max number of row for each parameter
             expect_equal(reqmaxvalues(), 1)
             # required min number of row for each parameter
             expect_equal(reqminvalues(), 1)
             # numeric columns
             expect_equal(numcol(), c("misura", "extunc"))
             # number of numeric columns
             expect_equal(sumnum(), 2)
             # sumnum is TRUE
             expect_equal(numok(), TRUE)
             # character columns
             expect_equal(charcol(), "analita")
             # number of factor columns
             expect_equal(sumchar(), 1)
             # charok is TRUE
             expect_equal(charok(), TRUE)
             # dataloaded flag
             expect_equal(dataloaded(), "dataloaded")

             session$setInputs(
               parvar = "analita",
               responsevar = "misura",
               uncertaintyvar = "extunc"
             )

             # maxvalues is less or equal 1
             expect_true(maxvalues() <= 1)
             # minvalues is greater or equal 1
             expect_true(minvalues() >= 1)
             # valuesok is TRUE
             expect_equal(valuesok(), TRUE)
             # dataok is TRUE
             expect_equal(dataok(), TRUE)
             # isunc is with_unc
             expect_equal(isunc(), "with_unc")
             # is2measures is 2measures
             expect_equal(is2measures(), "not_2measures")
             # parlist is yields
             expect_equal(parlist(), factor(c("boro", "ferro")))

             session$setInputs(nextbtn = 1,
                               yesbtn = 1)

             # saved output
             expect_equal(class(r$loadfile02), "reactivevalues")
             expect_equal(r$loadfile02$data, datafile())
             expect_equal(r$loadfile02$parvar, "analita")
             expect_equal(r$loadfile02$parlist, factor(c("boro", "ferro")))
             expect_equal(r$loadfile02$responsevar, "misura")
             expect_equal(r$loadfile02$secondresponsevar, NULL)
             expect_equal(r$loadfile02$uncertaintyvar, "extunc")

           })
################### TO BE UPDATED ###################
# testing errors for 2samples and wrong number of groups, columns and rows
testServer(mod_loadfile02_server,
           # Module params
           args = list(r = r), {
             r$aim01$aim <- "2samples"
             session$flushReact()

             ns <- session$ns

             # 1 group instead of 2
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample.csv", package = "SIprecisa"),
               name = "raw_1sample.csv"
             ))

             expect_error(groupok())

             # 3 groups on some parameters instead of 2 on all parameters
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2samples_wronggroups.csv", package = "SIprecisa"),
               name = "raw_2samples_wronggroups.csv"
             ))

             expect_error(groupok())

             # no grouping variable
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_nogroups.csv", package = "SIprecisa"),
               name = "raw_nogroups.csv"
             ))

             expect_error(charok())
             expect_error(groupok())

             # 2 numerical variable instead of 1
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc.csv", package = "SIprecisa"),
               name = "raw_2values_unc.csv"
             ))

             expect_error(numok())

             # 5 values for a parameter and group pair insted of a minimum of 6 values
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2samples_4rows.csv", package = "SIprecisa"),
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
               datapath = system.file("extdata", "raw_2samples.csv", package = "SIprecisa"),
               name = "raw_2samples.csv"
             ))

             expect_error(groupok())

             # 3 groups on some parameters instead of 1 on all parameters
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample_wronggroups.csv", package = "SIprecisa"),
               name = "raw_1sample_wronggroups.csv"
             ))

             expect_error(groupok())

             # no grouping variable
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_nogroups.csv", package = "SIprecisa"),
               name = "raw_nogroups.csv"
             ))

             expect_error(charok())
             expect_error(groupok())

             # 2 numerical variable instead of 1
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc.csv", package = "SIprecisa"),
               name = "raw_2values_unc.csv"
             ))

             expect_error(numok())
             expect_error(groupok())

             # 5 values for a parameter and group pair insted of a minimum of 6 values
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample_4rows.csv", package = "SIprecisa"),
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
               datapath = system.file("extdata", "raw_2samples.csv", package = "SIprecisa"),
               name = "raw_2samples.csv"
             ))

             expect_error(numok())

             # 3 groups on some parameters instead of 1 on all parameters
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_2values_unc_wronggroups.csv", package = "SIprecisa"),
               name = "raw_2values_unc_wronggroups.csv"
             ))

             expect_error(groupok())

             # no grouping variable
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_nogroups.csv", package = "SIprecisa"),
               name = "raw_nogroups.csv"
             ))

             expect_error(numok())
             expect_error(charok())
             expect_error(groupok())

             # 2 values for a parameter and group pair insted of a minimum of 1 value
             session$setInputs(file = list(
               datapath = system.file("extdata", "raw_1sample_4rows.csv", package = "SIprecisa"),
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

