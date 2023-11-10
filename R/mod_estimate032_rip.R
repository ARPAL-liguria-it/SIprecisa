#' estimate UI Function: option for repeatability estimates from paired
#' measurement values
#'
#' @description A shiny Module for estimating performance parameters of
#'   chemical analytical methods.
#'   Differences between data values are checked for normality, presence of outliers,
#'   and repeatability parameters are calculated.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Standard deviation is estimated by dividing the mean of the differences bewteen
#'   each pair of values by 1.128, according to ISO 8258 and ISO 11352:2012, Annex A.
#'
#'   Test results are formatted in HTML.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return Two UI {shiny} input widgets:
#' \itemize{
#'  \item{udm}{a text input for the unit of measurements.
#'    It is used for axes description and reporting, it is optional and it can be left blank.}
#'  \item{significance}{a radiobutton widgted with test confidence levels.
#'    Choices are "90\%", "95\%", "99\%", default is "95\%".}
#' }
#'
#' @noRd
#'
#' @import shiny
mod_estimate032_rip_inputs_ui <- function(id) {
  ns <- NS(id)
  tagList(

      # 1. write the unit of measurment (optional)
      textInput(ns("udm"),
                "Unit\u00E0 di misura",
                ""),

      # 2. select the test significant level
      radioButtons(
        ns("significance"),
        "Livello di confidenza",
        choices = c(
          "90%" = 0.90,
          "95%" = 0.95,
          "99%" = 0.99
        ),
        selected = 0.95
      )

  )
}

#' estimate UI Function: option for repeatability estimates from paired
#' measurement values
#'
#' @description A shiny Module for estimating performance parameters of
#'   chemical analytical methods.
#'   Differences between data values are checked for normality, presence of outliers,
#'   and repeatability parameters are calculated.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Standard deviation is estimated by dividing the mean of the differences bewteen
#'   each pair of values by 1.128, according to ISO 8258 and ISO 11352:2012, Annex A.
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
mod_estimate032_rip_output_ui <- function(id) {
  ns <- NS(id)
  tagList(

    bslib::navset_hidden(
    id = ns("help_results"),

    bslib::nav_panel("help",
      help_card(
        card_title = "Cosa devi fare",
        rmdfile = "help_estimate032_rip.Rmd",
        rmdpackage = "SIprecisa"
      )
    ),

    bslib::nav_panel("results",

      bslib::layout_columns(
        bslib::card(
          bslib::card_header(icon("vials"), "Boxplot e tabella riassuntiva delle differenze tra valori"),

          bslib::layout_column_wrap(
            width = NULL,
            style = htmltools::css(grid_template_columns = "1fr 2fr"),
            bslib::card_body(plotly::plotlyOutput(ns("boxplot"))),
            bslib::card_body(plotly::plotlyOutput(ns("confint")))
          ),

          bslib::card_body(DT::DTOutput(ns("summarytable")))
        ),

        bslib::navset_card_tab(
          id = ns("tabresults"),
          title = list(icon("vials"), "Test statistici e parametri prestazionali"),

          bslib::nav_panel("Normalit\u00E0",
            h4("Test per la verifica della normalit\u00E0 (Shapiro-Wilk)"),
            htmlOutput(ns("shapirotest")),
            hr(),
            h4("Test per identificare possibili outliers (GESD)"),
            htmlOutput(ns("outliers"))
          ),
          bslib::nav_panel("Precisione",
            htmlOutput(ns("precision"))
          )
        )
      )
    )

  ))
}

#' estimate server Function: option for repeatability estimates from paired
#' measurement values
#'
#' @description A shiny Module for estimating performance parameters of
#'   chemical analytical methods.
#'   Differences between data values are checked for normality, presence of outliers,
#'   and repeatability parameters are calculated.
#'
#' @details Normality is checked by using the Shapiro-Wilk test.
#'   Possible outliers are inspected by generalized extreme studentized deviate test.
#'   Standard deviation is estimated by dividing the mean of the differences bewteen
#'   each pair of values by 1.128, according to ISO 8258 and ISO 11352:2012, Annex A.
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
#'    \item{secondresponsevar} is the data.frame column name in which the response numerical values for the duplicated measures are stored.
#'    }
#'In \code{r$estimate03$myparameter} the selected parameter name is stored;
#' @return A {plotly} interactive boxplot, a {DT} summary table
#'  and Shapiro-Wilk test results formatted in HTML.
#'  a reactiveValues \code{r$estimate03x} with the following items:
#'    \itemize{
#'      \item{parameter}{the selected parameter;}
#'      \item{udm}{the unit of measurement;}
#'      \item{significance}{the level of significance for the tests;}
#'      \item{data}{the subsetted dataset with a flag for removed or not removed values;}
#'      \item{summary}{a summary table;}
#'      \item{normality}{a Markdown formatted string with the results for the normality test.}
#'      \item{outliers}{a Markdown formatted string with the results for the outliers test.}
#'      \item{precision}{a Markdown formatted string with the precision performance parameters.}
#'    }
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @importFrom plotly renderPlotly
#' @importFrom DT renderDT
mod_estimate032_rip_server <- function(id, r) {
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

          updateRadioButtons(session,
                             "significance",
                             selected = r$estimate03[[r$estimate03$myparameter]]$significance)
          updateTextInput(session,
                          "udm",
                          value = r$estimate03[[r$estimate03$myparameter]]$udm)

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
        }
      })

    ## unit of measurement
    observeEvent(input$udm, ignoreNULL = FALSE, {
      udmclean <- gsub("[()\\[\\]]", "", input$udm, perl = TRUE)
      r$estimate03x$udm <- udmclean
    })

    ## test confidence level
    observeEvent(input$significance, ignoreNULL = FALSE, {
      r$estimate03x$significance <- input$significance
    })

    # preparing the reactive dataset for outputs ----
    mydata <- reactive({
      r$loadfile02$data[r$loadfile02$data[[r$loadfile02$parvar]] == r$estimate03x$parameter, ]
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
    observeEvent(r$estimate03$myparameter, {
      keys(NULL)
    })

    # flag per i punti selezionati
    is_outlier <- reactive(key() %in% keys())

    # assembling the dataframe
    input_data <- reactive({

      mydifference <- mydata()[[r$loadfile02$responsevar]] - mydata()[[r$loadfile02$secondresponsevar]]
      mymean <- mydata()[, rowMeans(.SD),
                         .SDcols = c(r$loadfile02$responsevar, r$loadfile02$secondresponsevar)]
      relresponse <- mydifference / mymean

      data.frame(
        key = key(),
        outlier = is_outlier(),
        measure1 = mydata()[[r$loadfile02$responsevar]],
        measure2 = mydata()[[r$loadfile02$secondresponsevar]],
        response = mydifference,
        rel_response = relresponse,
        abs_rel_response = abs(relresponse),
        abs_perc_response = 100 * abs(relresponse)
      )

    })

    # subset of non outliers
    selected_data <- reactive({
      req(input_data())

      input_data()[input_data()$outlier == FALSE,]
    })

    # min number of values
    minval <- reactive({
      req(selected_data())

      dim(selected_data())[1]
    })

    # reactive boxplot ----
    plotlyboxplot <- reactive({
      req(input_data())

      myboxplot <- boxplot_rip(
        data = input_data(),
        response = "rel_response",
        udm = "%"
      )

      myboxplot$x$source <- "boxplot"

      myboxplot

    })

    output$boxplot <- plotly::renderPlotly({

      # if results have been saved, restore the boxplot
      if(r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()){

        r$estimate03[[r$estimate03$myparameter]]$plotlyboxplot

        # else a new boxplot is calculated and shown
      } else {

        plotlyboxplot()
      }
      })

    # reactive R and mean X chart ----
    plotlyrchart <- reactive({
      req(input_data())

      myrchart <- shewart_rip(
        data = input_data(),
        measure1 = "measure1",
        measure2 = "measure2",
        udm = r$estimate03x$udm
      )

      myrchart$x$source <- "boxplot"

      myrchart

    })

    output$confint <- plotly::renderPlotly({

      # if results have been saved, restore the boxplot
      if(r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()){

        r$estimate03[[r$estimate03$myparameter]]$plotlyconfint

        # else a new boxplot is calculated and shown
      } else {

        plotlyrchart()
      }
    })


    # reactive summary table ----
    summarytable <- reactive({
      req(input_data())

      rowsummary_rip(
        data = input_data(),
        response = "abs_perc_response",
        udm = "%"
      )

    })

    output$summarytable <- DT::renderDT({
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


    # results of normality check ----
    shapiro_text <-
      "%s (W = %.3f, <i>p</i>-value = %.4f)</br>"

    shapiro_html <- reactive({
      # don't update the list if results have been already saved
      req(r$estimate03[[r$estimate03$myparameter]]$saved |> isFALSE() ||
            r$estimate03[[r$estimate03$myparameter]]$saved |> is.null())

    shapiro_output <- selected_data()[, "rel_response"] |>
      fct_shapiro()

        sprintf(
          shapiro_text,
          shapiro_output$result,
          shapiro_output$W,
          shapiro_output$pvalue
        )
    })

    output$shapirotest <- renderText({
      validate(
        need(minval() >= 6,
             message = "Servono almeno 6 valori per eseguire il test")
      )
      # if results have been saved, restore the normality test results
      if (r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()) {

        r$estimate03[[r$estimate03$myparameter]]$normality_html

      } else {

        shapiro_html()
      }
    })

    # results for outliers check ----
    out_text <-
      "%s a un livello di confidenza del 95%% </br> %s a un livello di confidenza del 99%% </br></br>"

    outliers_html <- reactive({
      req(selected_data())
      req(minval() >= 6)
      # don't update the list if results have been already saved
      req(r$estimate03[[r$estimate03$myparameter]]$saved |> isFALSE() ||
            r$estimate03[[r$estimate03$myparameter]]$saved |> is.null())

      outtest_output95 <- (selected_data()[, "rel_response"] * 100) |>
        fct_gesd(significance = 0.95, m = 2)

      outtest_output99 <- (selected_data()[, "rel_response"] * 100) |>
        fct_gesd(significance = 0.99, m = 2)

        sprintf(out_text,
                outtest_output95$text,
                outtest_output99$text)

    })

    output$outliers <- renderText({
      validate(
        need(minval() >= 6,
             message = "Servono almeno 6 valori per eseguire il test")
      )
      # if results have been saved, restore the outliers test results
      if (r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()) {

        r$estimate03[[r$estimate03$myparameter]]$outliers_html

      } else {

        outliers_html()
      }
    })

    # precision performances ----
    precision_results <- reactive({
      req(r$estimate03[[r$estimate03$myparameter]]$saved |> isFALSE() ||
            r$estimate03[[r$estimate03$myparameter]]$saved |> is.null())

      fct_precision_rip(data = selected_data(),
                        response = "abs_rel_response",
                        significance = r$estimate03x$significance |>
                          as.numeric())

    })

    precision_text <-
      "<ul>
  <li> Limite di ripetibilità relativo (per \u03b1 = %s) = %s &percnt;</li>
  <li> Coefficiente di variazione = %s &percnt;</li>
</ul>"

    precision_html <- reactive({

      sprintf(
        precision_text,
        precision_results()$alpha |> format_sigfig(3L),
        precision_results()$rel_repeatability |> format_sigfig(3L),
        precision_results()$rsd |> format_sigfig(3L)
      )

    })

    output$precision <- renderText({
      validate(
        need(minval() >= 8,
             message = "Servono almeno 8 valori per poter calcolare i parametri prestazionali")
      )
      # if results have been saved, restore the t-test results
      if (r$estimate03[[r$estimate03$myparameter]]$saved |> isTRUE()) {

        r$estimate03[[r$estimate03$myparameter]]$precision_html

      } else {

        precision_html()
      }

    })


    # saving the outputs ----

    observeEvent(precision_html(), {

      # output dataset
      r$estimate03x$data <- mydata()[, !r$loadfile02$parvar, with = FALSE]
      r$estimate03x$data[, "rimosso"] <- ifelse(is_outlier() == TRUE, "s\u00EC", "no")

      # summary table
      r$estimate03x$summary <- summarytable()

      # plots
      r$estimate03x$plotlyboxplot <- plotlyboxplot()
      r$estimate03x$plotlyconfint <- plotlyrchart()

      # test results
      r$estimate03x$normality <- shapiro_html()
      r$estimate03x$outliers <- outliers_html()
      r$estimate03x$precision <- precision_html()
      r$estimate03x$ttest <- NA
      r$estimate03x$trueness <- NA
      # flag for when ready to be saved
      r$estimate03x$click <- 1

      # the plot is saved only when the save button is clicked
    })

  })
}
