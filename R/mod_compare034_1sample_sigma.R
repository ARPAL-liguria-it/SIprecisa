#' compare UI Function: 1 sample vs known standard deviation option
#'
#' @description A shiny Module for basic one-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and variance
#'   comparison by hypothesis tesing.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Variance values are compared by chi-squared test.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return Five UI widgets:
#' \itemize{
#'  \item{udm}{a text input for the unit of measurements.
#'    It is used for axes description and reporting, it is optional and it can be left blank.}
#'  \item{sd}{a numeric input widget for typing the known reference standard deviation value.}
#'  \item{submit}{an action button to submit the sd value to calculations.}
#'  \item{alternative}{a radiobutton widget with alternative test hypothesis.
#'    Choices are "different" or "greater", default is "different".}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_compare034_1sample_sigma_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # 1. write the unit of measurment (optional)
    textInput(ns("udm"),
              "Unit\u00E0 di misura",
              ""),

    hr(style = "border-top: 1px solid #000000;"),

    # 2. label for the reference value
    textInput(
      ns("label"),
      "Nome del valore di riferimento",
      ""
    ),

    # 3. known reference standard deviation
    numericInput(
      ns("sd"),
      "Deviazione standard di riferimento",
      0,
      min = 0),

    # 4. submit button
    actionButton(
      ns("submit"),
      "Calcola",
      icon = icon("calculator")
      ),

    hr(style = "border-top: 1px solid #000000;"),

      # 5. select the test significant level
      radioButtons(
        ns("significance"),
        "Livello di confidenza",
        choices = c(
          "90%" = 0.90,
          "95%" = 0.95,
          "99%" = 0.99
        ),
        selected = 0.95
      ),

      # 6. select the test alternative hypothesis
      radioButtons(
        ns("alternative"),
        "Ipotesi alternativa",
        choices = c("\u2260" = "different",
                    ">" = "greater"),
        selected = "different"
    )

  )
}

#' compare UI Function: 1 sample vs known standard deviation option
#'
#' @description A shiny Module for basic one-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and
#'   standad deviation comparison by hypothesis tesing.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   standard deviation values are compared by the chi^2-test.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return A two column fluidrow. In the first column a boxplot and a summary table
#' are reported, whereas in the second column test results are reported in two
#' separate tabs.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib navset_hidden nav_panel card card_header card_body layout_columns navset_card_tab
mod_compare034_1sample_sigma_output_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::navset_hidden(
    id = ns("help_results"),

    bslib::nav_panel("help",
      help_card(
        card_title = "Cosa devi fare",
        rmdfile = "help_compare034_1sample_sigma.Rmd",
        rmdpackage = "SIconfronta"
      )
    ),

    bslib::nav_panel("results",

      bslib::layout_columns(
        bslib::card(
          bslib::card_header(icon("vials"), "Boxplot e tabella riassuntiva"),
          bslib::card_body(plotly::plotlyOutput(ns("boxplot")),),
          bslib::card_body(DT::DTOutput(ns("summarytable")))
        ),

        bslib::navset_card_tab(
          id = ns("tabresults"),
          title = list(icon("vials"), "Test statistici"),

          bslib::nav_panel("Normalit\u00E0",
            h4("Test per la verifica della normalit\u00E0 (Shapiro-Wilk)"),
            htmlOutput(ns("shapirotest")),
            hr(),
            h4("Test per identificare possibili outliers (GESD)"),
            htmlOutput(ns("outliers"))
          ),
          bslib::nav_panel("Varianze",
            h4("Test per il rapporto tra varianza sperimentale e un valore noto (\u03C7\u00B2-test)"),
            htmlOutput(ns("chitest"))
          )
        )
      )
    )
  ))
}

#' compare Server Function
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#'   The module allows to select the confidence level and the tests alternative
#'   hypothesis. Data are checked for normality, presence of outliers and
#'   variance comparison hypothesis tests are performed.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} storing data produced in the other modules.
#' in \code{r$loadfile02$} the following values can be found:
#' \itemize{
#'    \item{data} is the imported data.frame;
#'    \item{parvar} is the data.frame column name in which parameters are stored;
#'    \item{parlist} is the list of parameters provided by the data.frame;
#'    \item{groupvar} is the data.frame column name in which groub labels are stored;
#'    \item{responsevar} is the data.frame column name in which the response numerical values are stored.
#'    }
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and Shapiro-Wilk test and \eqn{$chi$^2}-test results formatted in HTML.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare034_1sample_sigma_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    r$compare03x <- reactiveValues()

    # storing input values into r$compare03x reactiveValues ----

    ## selected parameter
    observeEvent(r$compare03$myparameter, {
      r$compare03x$parameter <- r$compare03$myparameter

      # updating the tabset switching from help to results tabs
      help_results <- ifelse(r$compare03x$parameter == "", "help", "results")
      updateTabsetPanel(inputId = "help_results", selected = help_results)
    })

    ## reference label and sd
    observeEvent(input$submit, ignoreInit = TRUE, {
      r$compare03x$label2 <- input$label
      r$compare03x$sd2 <- input$sd

      # validate the input
      if (r$compare03x$label2 != "" &
          is.character(r$compare03x$label2) &
          is.numeric(r$compare03x$sd2) &
          r$compare03x$sd2 > 0) {

        r$compare03x$click <- 1

      } else {

        r$compare03x$click <- 0
      }
    })

    ## reset reference label and sd after changing the parameter
    observeEvent(r$compare03$myparameter, ignoreNULL = FALSE, {

      if(r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()){

        freezeReactiveValue(input, "alternative")
        freezeReactiveValue(input, "significance")
        freezeReactiveValue(input, "udm")
        freezeReactiveValue(input, "label")
        freezeReactiveValue(input, "sd")

        updateRadioButtons(session,
                           "alternative",
                           selected = r$compare03[[r$compare03$myparameter]]$alternative)
        updateRadioButtons(session,
                           "significance",
                           selected = r$compare03[[r$compare03$myparameter]]$significance)
        updateTextInput(session,
                        "udm",
                        value = r$compare03[[r$compare03$myparameter]]$udm)
        updateTextInput(session,
                        "label",
                        value = r$compare03[[r$compare03$myparameter]]$label2)
        updateNumericInput(session,
                           "sd",
                           value = r$compare03[[r$compare03$myparameter]]$sd2)

        showModal(
          modalDialog(
            title = "Hai gi\u00E0 salvato i risultati",
            shiny::HTML(
              "Per sovrascrivere i risultati salvati, clicca sui pulsanti
            cancella e poi nuovamente su salva, altrimenti le modifiche andranno perse.
            <br>
            Trovi i pulsanti in basso nella barra dei comandi,
            nella parte sinistra della pagina"
            ),
            easyClose = TRUE,
            footer = modalButton("Va bene")
          )
        )

        r$compare03x$click <- 1

        # else, just use the default initial values
      } else {
      freezeReactiveValue(input, "label")
      freezeReactiveValue(input, "sd")
      updateTextInput(session, "label", value = "")
      updateNumericInput(session, "sd", value = 0)

      r$compare03x$click <- 0

    }
    })

    ## unit of measurement
    observeEvent(input$udm, ignoreNULL = FALSE, {
      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$compare03x$udm <- udmclean
    })

    ## alternative hypothesis
    observeEvent(input$alternative, ignoreNULL = FALSE, {
      r$compare03x$alternative <- input$alternative
    })

    ## test confidence level
    observeEvent(input$significance, ignoreNULL = FALSE, {
      r$compare03x$significance <- input$significance
    })


    # preparing the reactive dataset for outputs ----
    mydata <- reactive({
      r$loadfile02$data[get(r$loadfile02$parvar) == r$compare03x$parameter]
    })

    rownumber <- reactive({
      req(mydata())

      nrow(mydata())
    })

    key <- reactive(seq(from = 1, to = rownumber()))

    # using the row index to identify outliers
    keys <- reactiveVal()

    observeEvent(plotly::event_data("plotly_click", source = "boxplot"), {
      req(plotly::event_data("plotly_click", source = "boxplot")$key)

      key_new <- plotly::event_data("plotly_click", source = "boxplot")$key
      key_old <- keys()

      if (key_new %in% key_old) {
        keys(setdiff(key_old, key_new))
      } else {
        keys(c(key_new, key_old))
      }

    })

    # reset the keys index when changing the parameter
    observeEvent(r$compare03$myparameter, {
      keys(NULL)
    })

    # flag per i punti selezionati
    is_outlier <- reactive(key() %in% keys())

    # assembling the dataframe
    input_data <- reactive({

      data.frame(
        key = key(),
        outlier = is_outlier(),
        response = mydata()[[r$loadfile02$responsevar]],
        group = mydata()[[r$loadfile02$groupvar]]
      )

    })

    # subset of non outliers
    selected_data <- reactive({
      req(input_data())

      input_data()[input_data()$outlier == FALSE,]
    })

    # min number of values for the two groups
    minval <- reactive({
      req(!is.null(selected_data()))

      sapply(levels(selected_data()$group),
             function(x) {
               selected_data()[selected_data()$group == x, ] |>
                 nrow()
             }) |>
        min()
    })


    # reactive boxplot ----
    plotlyboxplot <- reactive({
      req(input_data())

      boxplot_1sample_sigma(
        data = input_data(),
        group = r$loadfile02$groupvar,
        response = r$loadfile02$responsevar,
        reflabel = r$compare03x$label2,
        reference = r$compare03x$sd2,
        udm = r$compare03x$udm
      )

    })

    output$boxplot <- plotly::renderPlotly({
      validate(
        need(r$compare03x$click == 1,
             message =
               "Inserisci il nome e il valore di riferimento, poi premi 'Calcola'"),
        need(ifelse(r$compare03x$click == 0, TRUE, ifelse(is.numeric(r$compare03x$sd2), TRUE, FALSE)),
             message = "Il valore di riferimento deve essere un valore numerico"),
        need(ifelse(r$compare03x$click == 0, TRUE, ifelse(r$compare03x$label2 != "", TRUE, FALSE)),
             message = "Il nome del valore di riferimento deve essere una stringa di caratteri")
      )

      # if results were saved, restore the boxplot
      if(r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()){

        r$compare03[[r$compare03$myparameter]]$plotlyboxplot

        # else a new boxplot is calculated and shown
      } else {

        plotlyboxplot()
      }
    })


    # reactive summary table ----
    summarytable <- reactive({
      req(selected_data())
      req(r$compare03x$label2)

      rowsummary_1sample_sigma(
        data = selected_data(),
        group = "group",
        response = "response",
        reflabel = r$compare03x$label2,
        reference = r$compare03x$sd2,
        udm = r$compare03x$udm
      )

    })

    output$summarytable <- DT::renderDT({
      validate(
        need(r$compare03x$click == 1,
             message = FALSE)
      )
      # if results have been saved, restore the summarytable
      if (r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()) {

        DT::datatable(r$compare03[[r$compare03$myparameter]]$summary,
                      style = "bootstrap4",
                      fillContainer = TRUE,
                      options = list(dom = "t"),
                      rownames = FALSE)
      } else {

        DT::datatable(summarytable(),
                      style = "bootstrap4",
                      fillContainer = TRUE,
                      options = list(dom = "t"),
                      rownames = FALSE)
      }

    })


    # results of normality check ----
    shapiro_text <-
      "<b>Gruppo %s:</b> %s (W = %.3f, <i>p</i>-value = %.4f)</br>"

    # levels of the grouping factor
    lvl <- reactive({
      req(selected_data())

      levels(selected_data()$group)
    })

    shapirotest_list <- reactive({
      req(lvl())
      # don't update the list if results have been already saved
      req(r$compare03[[r$compare03$myparameter]]$saved |> isFALSE() ||
            r$compare03[[r$compare03$myparameter]]$saved |> is.null())

      sapply(lvl(), function(x) {
        shapiro_output <- selected_data()[which(selected_data()$group == x),
                                          "response"] |>
          fct_shapiro()

        sprintf(
          shapiro_text,
          x,
          shapiro_output$result,
          shapiro_output$W,
          shapiro_output$pvalue
        )
      })
    })

    shapiro_html <- reactive(paste(shapirotest_list(), collapse = ""))

    output$shapirotest <- renderText({
      validate(
        need(minval() >= 5,
             message = "Servono almeno 5 valori per poter eseguire i test")
      )
      # if results have been saved, restore the normality test results
      if (r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()) {

        r$compare03[[r$compare03$myparameter]]$normality_html

      } else {

        shapiro_html()
      }
    })


    # results for outliers check ----
    out_text <-
      "<b>Gruppo %s:</b></br> %s a un livello di confidenza del 95%% </br> %s a un livello di confidenza del 99%% </br></br>"

    outtest_list <- reactive({
      req(selected_data())
      req(minval() >= 5)
      # don't update the list if results have been already saved
      req(r$compare03[[r$compare03$myparameter]]$saved |> isFALSE() ||
            r$compare03[[r$compare03$myparameter]]$saved |> is.null())

      sapply(lvl(), function(x) {
        outtest_output95 <-
          selected_data()[which(selected_data()$group == x), "response"] |>
          fct_gesd(significance = 0.95)

        outtest_output99 <-
          selected_data()[which(selected_data()$group == x), "response"] |>
          fct_gesd(significance = 0.99)

        sprintf(out_text,
                x,
                outtest_output95$text,
                outtest_output99$text)
      })

    })

    outliers_html <- reactive(paste(outtest_list(), collapse = ""))

    output$outliers <- renderText({
      validate(
        need(minval() >= 5,
             message = "Servono almeno 5 valori per poter eseguire i test")
      )
      # if results have been saved, restore the outliers test results
      if (r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()) {

        r$compare03[[r$compare03$myparameter]]$outliers_html

      } else {

        outliers_html()
      }
    })


    #### results for the chi^2-test ----
    chitest_list <- reactive({
      req(selected_data())
      req(r$compare03x$significance)
      req(r$compare03x$alternative)
      req(r$compare03x$click == 1)
      # don't update if results have been saved
      req(r$compare03[[r$compare03$myparameter]]$saved |> isFALSE() ||
            r$compare03[[r$compare03$myparameter]]$saved |> is.null())

      fct_chitest_1sample_sigma(
        data = selected_data(),
        response = "response",
        group = "group",
        reflabel = r$compare03x$label2,
        reference = r$compare03x$sd2,
        significance = as.numeric(r$compare03x$significance),
        alternative = r$compare03x$alternative
      )

    })

    chitest_text <-
      "<b>H0:</b> %s </br>
<b>H1:</b> %s
<ul>
  <li> Deviazione standard dei valori (valore e intervallo di confidenza) = %s %s, %s \u2013 %s %s</li>
  <li> \u03C7<sup>2</sup> sperimentale = %s </li>
  <li> \u03C7<sup>2</sup> critico (\u03b1 = %s, \u03bd = %s) = %s </li>
  <li> <i>p</i>-value = %s </li>
</ul>
\u21e8 %s"

    chitest_html <- reactive({
      req(r$compare03x$click == 1)

      sprintf(
        chitest_text,
        chitest_list()$hypotheses[[1]],
        chitest_list()$hypotheses[[2]],
        chitest_list()$ratio[[1]],
        r$compare03x$udm,
        chitest_list()$ratio[[2]],
        chitest_list()$ratio[[3]],
        r$compare03x$udm,
        chitest_list()$test[[3]],
        chitest_list()$test[[2]],
        chitest_list()$test[[1]],
        chitest_list()$test[[4]],
        chitest_list()$test[[5]],
        chitest_list()$result
      )

    })

    output$chitest <- renderText({
      validate(
        need(minval() >= 5,
             message = "Servono almeno 5 valori per poter eseguire i test"),
        need(r$compare03x$click == 1,
             message =
               "Inserisci il nome e il valore di riferimento, poi premi 'Calcola'"),
        need(ifelse(r$compare03x$click == 0, TRUE, ifelse(is.numeric(r$compare03x$sd2), TRUE, FALSE)),
             message = "La deviazione standard deve essere un valore numerico"),
        need(ifelse(r$compare03x$click == 0, TRUE, ifelse(r$compare03x$label2 != "", TRUE, FALSE)),
             message = "Il nome del valore di riferimento deve essere una stringa di caratteri")
      )
      # if results have been saved, restore the chi2-test results
      if (r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()) {

        r$compare03[[r$compare03$myparameter]]$ftest_html

      } else {

        chitest_html()
      }
    })


    # saving the outputs ----
    trigger <- reactive({
      chitest_html()
      r$compare03x$udm
    })

    observeEvent(trigger(), {

      # output dataset
      r$compare03x$data <- mydata()[, !r$loadfile02$parvar, with = FALSE]
      r$compare03x$data[, "rimosso"] <- ifelse(is_outlier() == TRUE, "s\u00EC", "no")

      # summary table
      r$compare03x$summary <- summarytable()

      # boxplot
      r$compare03x$plotlyboxplot <- plotlyboxplot()

      # test results
      r$compare03x$normality <- shapiro_html()
      r$compare03x$outliers <- outliers_html()
      r$compare03x$ftest <- chitest_html()
      r$compare03x$ttest <- NA

      # the plot is saved only when the save button is clicked
    })

  })
}
