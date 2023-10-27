#' estimate UI Function: option forrecovery estimates
#' on a single measurement value
#'
#' @description A shiny Module for estimating recovery parameters of
#'   chemical analytical methods.
#'
#' @details The measurement value is compared with the reference value by
#' normalized error - En.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return Three UI {shiny} input widgets:
#' \itemize{
#'  \item{udm}{a text input for the unit of measurements.
#'    It is used for axes description and reporting, it is optional and it can be left blank.}
#'  \item{refvalue}{a numeric input widget for typing the known reference value.}
#'  \item{refuncertainty}{a numeric input widget for typing the extended uncertainty for
#'  the reference value.}
#'  \item{submit}{an action button to submit the reference value and uncertainty to calculations.}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_estimate033_recuno_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

      # 1. write the unit of measurment (optional)
      textInput(ns("udm"),
                "Unit\u00E0 di misura",
                ""),

      # 2. known reference mean value
      numericInput(
        ns("refvalue"),
        "Valore di riferimento",
        0,
        min = 0),

      # 3. known reference extended uncertainty value
      numericInput(
        ns("refuncertainty"),
        "Incertezza estesa",
        0,
        min = 0),

      # 4. submit button
      actionButton(
        ns("submit"),
        "Calcola",
        icon = icon("calculator")
      ),

  )
}

#' estimate UI Function: option forrecovery estimates
#' on a single measurement value
#'
#' @description A shiny Module for estimating recovery parameters of
#'   chemical analytical methods.
#'
#' @details The measurement value is compared with the reference value by
#' normalized error - En.
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
#' @import markdown
#' @importFrom plotly plotlyOutput
#' @importFrom DT DTOutput
#' @importFrom bslib navset_hidden nav_panel card card_header card_body layout_columns navset_card_tab layout_column_wrap
#' @importFrom htmltools css
mod_estimate033_recuno_output_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::navset_hidden(
    id = ns("help_results"),

    bslib::nav_panel("help",
      help_card(
        card_title = "Cosa devi fare",
        rmdfile = "help_estimate031_riprec.Rmd",
        rmdpackage = "SIprecisa"
      )
    ),

    bslib::nav_panel("results",

      bslib::layout_columns(
        bslib::card(
          bslib::card_header(icon("vials"), "Boxplot e tabella riassuntiva dei valori misurati"),

          bslib::card_body(
            bslib::card_body(plotly::plotlyOutput(ns("boxplot")))
          ),

          bslib::card_body(
            DT::DTOutput(ns("summarytable")))
        ),

        bslib::navset_card_tab(
          id = ns("tabresults"),
          title = list(icon("vials"), "Test statistici e parametri prestazionali"),

          bslib::nav_panel("Giustezza",
            htmlOutput(ns("trueness")),
            hr(),
            htmlOutput(ns("ttest"))
          )
        )
      )
    )

  ))
}

#' estimate server Function: option forrecovery estimates
#' on a single measurement value
#'
#' @description A shiny Module for estimating recovery parameters of
#'   chemical analytical methods.
#'
#' @details The measurement value is compared with the reference value by
#' normalized error - En.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Mean values of measurments is compared with the reference value by two-sided t-test.
#'
#'    This module is supposed to be used as {mod_estimate03} submodule.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} storing data produced in the other modules.
#' in \code{r$loadfile02$} the following values can be found:
#' \itemize{
#'    \item{data} is the imported data.frame;
#'    \item{parvar} is the data.frame column name in which parameters are stored;
#'    \item{parlist} is the list of parameters provided by the data.frame;
#'    \item{responsevar} is the data.frame column name in which the response numerical values are stored.
#'    }
#'In \code{r$estimate03$myparameter} the selected parameter name is stored;
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and Shapiro-Wilk test and \eqn{t}-test results formatted in HTML.
#'  a reactiveValues \code{r$estimate03x} with the following items:
#'    \itemize{
#'      \item{parameter}{the selected parameter;}
#'      \item{udm}{the unit of measurement;}
#'      \item{significance}{the level of significance for the tests;}
#'      \item{data}{the subsetted dataset with a flag for removed or not removed values;}
#'      \item{summary}{a summary table;}
#'      \item{trueness}{recovery performace parameters;}
#'      \item{entest}{a Markdown formatted string with the results for the t-test.}
#'    }
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly
#' @importFrom DT renderDT
mod_estimate033_recuno_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r$estimate03x <- reactiveValues()

    # storing input values into r$estimate03x reactiveValues ----

    ## selected parameter
    observeEvent(r$estimate03$myparameter, {
      r$estimate03x$parameter <- r$estimate03$myparameter

      # updating the tabset switching from help to results tabs
      help_results <- ifelse(r$estimate03x$parameter == "", "help", "results")
      updateTabsetPanel(inputId = "help_results", selected = help_results)
    })

      ## reset reference value and extended uncertainty after changing the parameter
      observeEvent(r$estimate03$myparameter, ignoreNULL = FALSE, {
        # if the results have been saved, restore the input values
        if(r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()){

          freezeReactiveValue(input, "significance")
          freezeReactiveValue(input, "udm")
          freezeReactiveValue(input, "refvalue")
          freezeReactiveValue(input, "refuncertainty")

          updateRadioButtons(session,
                             "significance",
                             selected = r$estimate03[[r$estimate03$myparameter]]$significance)
          updateTextInput(session,
                          "udm",
                          value = r$estimate03[[r$estimate03$myparameter]]$udm)
          updateNumericInput(session,
                             "refvalue",
                             value = r$estimate03[[r$estimate03$myparameter]]$refvalue)
          updateNumericInput(session,
                             "refuncertainty",
                             value = r$estimate03[[r$estimate03$myparameter]]$refuncertainty)

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

          r$estimate03x$click <- 1

          # else, just use the default initial values
        } else {

          freezeReactiveValue(input, "refvalue")
          freezeReactiveValue(input, "refuncertainty")

          updateNumericInput(session, "refvalue", value = 0)
          updateNumericInput(session, "refuncertainty", value = 0)

          r$estimate03x$click <- 0
        }

      })

    ## unit of measurement
    observeEvent(input$udm, ignoreNULL = FALSE, {
      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$estimate03x$udm <- udmclean
    })

    ## reference value
    observeEvent(input$refvalue, ignoreNULL = FALSE, {
      r$estimate03x$refvalue <- input$refvalue
      r$estimate03x$click <- ifelse(r$estimate03x$click == 1, 0, 0)
    })

    ## extended uncertainty of the reference value
    observeEvent(input$refuncertainty, ignoreNULL = FALSE, {
      r$estimate03x$refuncertainty <- input$refuncertainty
      r$estimate03x$click <- ifelse(r$estimate03x$click == 1, 0, 0)
    })

    #### conditions for trueness results ----
    ok_click <- reactive({
      ifelse(r$estimate03x$refvalue != 0 && r$estimate03x$click == 0,
             0, 1)
    })

    ok_calc <- reactive({
      ifelse(r$estimate03x$refvalue == 0,
             0, 1)
    })



    # preparing the reactive dataset for outputs ----
    mydata <- reactive({
      r$loadfile02$data[r$loadfile02$data[[r$loadfile02$parvar]] == r$estimate03x$parameter, ]
    })

    # assembling the dataframe
    input_data <- reactive({

      data.frame(
        response = mydata()[[r$loadfile02$responsevar]],
        uncertainty = mydata()[[r$loadfile02$uncertaintyvar]]
      )

    })

    # min number of values
    minval <- reactive({
      req(input_data())

      dim(input_data())[1]
    })

    ## reference value and uncertainty
    observeEvent(input$submit, ignoreInit = TRUE, {
      r$estimate03x$refvalue <- input$refvalue
      r$estimate03x$refuncertainty <- input$refuncertainty

      # validate the input
      if (r$estimate03x$refvalue |> is.numeric() &
          r$estimate03x$refuncertainty |> is.numeric() &
          r$estimate03x$refvalue > 0 &
          r$estimate03x$refuncertainty > 0) {

        r$estimate03x$click <- 1

      } else {

        r$estimate03x$click <- 0
      }
    })


    # reactive boxplot ----
    plotlyboxplot <- reactive({
      req(input_data())
      req(r$estimate03x$click == 1)

      myboxplot <- boxplot_recuno(
        data = input_data(),
        response = r$loadfile02$responsevar,
        uncertainty = r$loadfile02$uncertaintyvar,
        refvalue = r$estimate03x$refvalue,
        refuncertainty = r$estimate03x$refuncertainty,
        udm = r$estimate03x$udm
      )

      myboxplot$x$source <- "boxplot"

      myboxplot

    })

    output$boxplot <- plotly::renderPlotly({
      validate(
        need(minval() >= 1,
             message = "Serve almeno un valore per poter calcolare i parametri prestazionali"),
        need(ok_click() == 1, "Clicca Calcola per aggiornare i risultati."),
        need(ok_calc() == 1, "Serve un valore di riferimento per questo risultato")
      )

      # if results have been saved, restore the boxplot
      if(r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()){

        r$estimate03[[r$estimate03$myparameter]]$plotlyboxplot

        # else a new boxplot is calculated and shown
      } else {

        plotlyboxplot()
      }
      })


    # reactive summary table ----
    summarytable <- reactive({
      req(input_data())
      req(r$estimate03x$click == 1)

      rowsummary_recuno(
        data = input_data(),
        response = "response",
        uncertainty = "uncertainty",
        refvalue = r$estimate03x$refvalue,
        refuncertainty = r$estimate03x$refuncertainty,
        udm = r$estimate03x$udm
      )

    })

    output$summarytable <- DT::renderDT({
      validate(
        need(minval() >= 1,
             message = "Serve almeno un valore per poter calcolare i parametri prestazionali"),
        need(ok_click() == 1, "Clicca Calcola per aggiornare i risultati."),
        need(ok_calc() == 1, "Serve un valore di riferimento per questo risultato")
      )
      # if results have been saved, restore the summarytable
      if (r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()) {

        DT::datatable(r$estimate03[[r$estimate03$myparameter]]$summary,
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


    # results for the En-test ----
    entest_list <- reactive({
      req(input_data())
      req(r$estimate03x$click == 1)
      # don't update if results have been saved
      req(r$estimate03[[r$estimate03$myparameter]]$saved |> isFALSE() ||
          r$estimate03[[r$estimate03$myparameter]]$saved |> is.null())

      fct_entest_recuno(
        data = input_data(),
        response = "response",
        uncertainty = "uncertainty",
        refvalue = r$estimate03x$refvalue,
        refuncertainty = r$estimate03x$refuncertainty
        )

    })


    entest_text <-
"<h4> Test per valutare la presenza di bias (E number) </h4>
<b>H0:</b> %s </br>
<b>H1:</b> %s
<ul>
  <li> Differenza tra i due valori (valore e intervallo di confidenza) = %s %s, %s \u2013 %s %s</li>
  <li> E<sub>n</sub> sperimentale = %s </li>
  <li> E<sub>n</sub> critico = %s </li>
</ul>
\u21e8 %s"

    entest_html <- reactive({

      sprintf(
        entest_text,
        entest_list()$hypotheses[1],
        entest_list()$hypotheses[2],
        entest_list()$difference[1],
        r$estimate03x$udm,
        entest_list()$difference[2],
        entest_list()$difference[3],
        r$estimate03x$udm,
        entest_list()$test[1],
        entest_list()$test[2],
        entest_list()$result
      )

    })

    output$ttest <- renderText({
      validate(
        need(minval() >= 1,
             message = "Serve almeno un valore per poter calcolare i parametri prestazionali"),
        need(r$estimate03x$refvalue != 0, message = "")
      )
      # if results have been saved, restore the t-test results
      if (r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()) {

        r$estimate03[[r$estimate03$myparameter]]$ttest_html

      } else {

        entest_html()
      }

      })


    # trueness performances ----
    trueness_results <- reactive({
      req(r$estimate03x$click == 1)

      fct_trueness_recuno(data = input_data(),
                          response = "response",
                          refvalue = r$estimate03x$refvalue)

    })

    trueness_text <-
"<ul>
  <li> Recupero = %s &percnt;</li>
  <li> Bias = %s %s</li>
  <li> Bias = %s &percnt;</li>
</ul>"

    trueness_html <- reactive({
      req(r$estimate03[[r$estimate03$myparameter]]$saved |> isFALSE() ||
            r$estimate03[[r$estimate03$myparameter]]$saved |> is.null())

      sprintf(
        trueness_text,
        trueness_results()$recovery |> format_sigfig(3L),
        trueness_results()$bias |> format_sigfig(3L),
        r$estimate03x$udm,
        trueness_results()$relative_bias |> format_sigfig(3L)
      )

    })

    output$trueness <- renderText({
      validate(
        need(minval() >= 1,
             message = "Serve almeno un valore per poter calcolare i parametri prestazionali"),
          need(ok_click() == 1, "Clicca Calcola per aggiornare i risultati."),
          need(ok_calc() == 1, "Serve un valore di riferimento per questo risultato")
      )
      # if results have been saved, restore the t-test results
      if (r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()) {

        r$estimate03[[r$estimate03$myparameter]]$trueness_html

      } else {

       trueness_html()
      }

    })


    # saving the outputs ----

    observeEvent(trueness_html(), {

      # output dataset
      r$estimate03x$data <- mydata()[, !r$loadfile02$parvar, with = FALSE]

      # summary table
      r$estimate03x$summary <- summarytable()

      # plots
      r$estimate03x$plotlyboxplot <- plotlyboxplot()
      r$estimate03x$plotlyconfint <- NA

      # test results
      r$estimate03x$normality <- NA
      r$estimate03x$outliers <- NA
      r$estimate03x$trueness <- trueness_html()
      r$estimate03x$precision <- NA
      r$estimate03x$ttest <- entest_html()
      # flag for when ready to be saved
      r$estimate03x$click <- 1

      # the plot is saved only when the save button is clicked
    })

  })
}
