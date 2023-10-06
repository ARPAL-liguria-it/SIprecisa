#' compare UI Function: 2 values with given extended uncertainty option
#'
#' @description A shiny Module for comparing two measurement values with
#'  given extended uncertainy.
#'
#' @details Comparison is performed by \eq{E_n} calculation.
#'  The test result is formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return one UI widget {udm} in which the unit of measurements can be typed.
#'
#' @noRd
#'
#' @import shiny
mod_compare035_2values_unc_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

    # 1. write the unit of measurment (optional)
    textInput(ns("udm"),
              "Unit\u00E0 di misura",
              "")
  )
}

#' compare UI Function: 2 values with given extended uncertainty option
#'
#' @description A shiny Module for comparing two measurement values with
#'  given extended uncertainy.
#'
#' @details Comparison is performed by \eq{E_n} calculation.
#'  The test result is formatted in HTML.
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
mod_compare035_2values_unc_output_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::navset_hidden(
    id = ns("help_results"),

    bslib::nav_panel("help",
      help_card(
        card_title = "Cosa devi fare",
        rmdfile = "help_compare035_2values_unc.Rmd",
        rmdpackage = "SIconfronta"
      )
    ),

    bslib::nav_panel("results",

      bslib::layout_columns(
        bslib::card(
          bslib::card_header(icon("vials"), "Boxplot e tabella riassuntiva"),
          bslib::card_body(plotly::plotlyOutput(ns("boxplot")), ),
          bslib::card_body(DT::DTOutput(ns("summarytable")))
        ),

        bslib::navset_card_tab(
          id = ns("tabresults"),
          title = list(icon("vials"), "Test statistici"),

          bslib::nav_panel("Differenze",
            h4("Confronto tra valori (E number)"),
            htmlOutput(ns("ttest"))
          )
        )
      )
    )

  ))
}

#' compare Server Function: 2 values with given extended uncertainty option
#'
#' @description A shiny Module for comparing two measurement values with
#'  given extended uncertainy.
#'
#' @details Comparison is performed by \eq{E_n} calculation.
#'  The test result is formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} storing data produced in the other modules.
#' in \code{r$loadfile02$} the following values can be found:
#' \itemize{
#'    \item{data} is the imported data.frame;
#'    \item{parvar} is the data.frame column name in which parameters are stored;
#'    \item{parlist} is the list of parameters provided by the data.frame;
#'    \item{groupvar} is the data.frame column name in which groub labels are stored;
#'    \item{responsevar} is the data.frame column name in which the response
#'     numerical values are stored;
#'    \item{uncertaintyvar} is the data.frame column name in which the extended
#'     uncertainty numerical values are stored.
#'    }
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and \eq{E_n} test in HTML format.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly plot_ly add_boxplot add_markers layout config
mod_compare035_2values_unc_server <- function(id, r) {
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

      # if the results have been saved, restore the input values
      if(r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()){

        freezeReactiveValue(input, "udm")

        updateTextInput(session,
                        "udm",
                        value = r$compare03[[r$compare03$myparameter]]$udm)

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
      }
    })

    ## unit of measurement
    observeEvent(input$udm, ignoreNULL = FALSE, {
      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$compare03x$udm <- udmclean
    })

    # preparing the reactive dataset for outputs ----
    mydata <- reactive({
      r$loadfile02$data[get(r$loadfile02$parvar) == r$compare03x$parameter]
    })

    # assembling the dataframe
    input_data <- reactive({
      req(mydata())

      data.frame(
        response = mydata()[[r$loadfile02$responsevar]],
        uncertainty = mydata()[[r$loadfile02$uncertaintyvar]],
        group = mydata()[[r$loadfile02$groupvar]]
      )

    })

    # scatter plot with error bars ----
    plotlyboxplot <- reactive({
      req(input_data())

      boxplot_2values_unc(
        data = input_data(),
        group = r$loadfile02$groupvar,
        response = r$loadfile02$responsevar,
        uncertainty = r$loadfile02$uncertaintyvar,
        udm = r$compare03x$udm
      )

    })

    output$boxplot <- plotly::renderPlotly({
      # if results have been saved, restore the boxplot
      if(r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()){

        r$compare03[[r$compare03$myparameter]]$plotlyboxplot

        # else a new boxplot is calculated and shown
      } else {

        plotlyboxplot()
      }
    })


    # summary table ----
    summarytable <- reactive({
      req(input_data())

      rowsummary_2values_unc(
        data = input_data(),
        group = "group",
        response = "response",
        uncertainty = "uncertainty",
        udm = r$compare03x$udm
      )

    })


    output$summarytable <- DT::renderDT({
      # if results have been saved, restore the summarytable
      if (r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()) {

        DT::datatable(r$compare03[[r$compare03$myparameter]]$summary,
                      options = list(dom = "t"),
                      rownames = FALSE,
                      style = "bootstrap4",
                      fillContainer = TRUE)
      } else {

        DT::datatable(summarytable(),
                      options = list(dom = "t"),
                      rownames = FALSE,
                      style = "bootstrap4",
                      fillContainer = TRUE)
      }

    })

    #### results for the En-test ----
    entest_list <- reactive({
      # don't update if results have been saved
      req(r$compare03[[r$compare03$myparameter]]$saved |> isFALSE() ||
            r$compare03[[r$compare03$myparameter]]$saved |> is.null())

      fct_entest_2values_unc(
        data = input_data(),
        response = "response",
        group = "group",
        uncertainty = "uncertainty"
      )

    })

    entest_text <-
      "<b>H0:</b> %s </br>
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
        r$compare03x$udm,
        entest_list()$difference[2],
        entest_list()$difference[3],
        r$compare03x$udm,
        entest_list()$test[1],
        entest_list()$test[2],
        entest_list()$result
      )

    })

    output$ttest <- renderText({
      req(input_data())
      # if results have been saved, restore the En-test results
      if (r$compare03[[r$compare03$myparameter]]$saved |> isTRUE()) {

        r$compare03[[r$compare03$myparameter]]$ttest_html

      } else {

        entest_html()
      }
    })

    # saving the outputs ----
    observeEvent(entest_html(), {

      # output dataset
      r$compare03x$data <- mydata()[, !r$loadfile02$parvar, with = FALSE]

      # summary table
      r$compare03x$summary <- summarytable()

      # boxplot
      r$compare03x$plotlyboxplot <- plotlyboxplot()

      # test results
      r$compare03x$normality <- NA
      r$compare03x$outliers <- NA
      r$compare03x$ftest <- NA
      r$compare03x$ttest <- entest_html()
      # flag for when ready to be saved
      r$compare03x$click <- 1

      # the plot is saved only when the save button is clicked
    })

  })
}
