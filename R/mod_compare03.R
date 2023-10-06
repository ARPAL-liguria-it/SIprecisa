#' compare UI Function:
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#' The module subsets a {data.table}, loaded by the {mod_loadfile02} module of the
#' {SIconfronta} package, by a single parameter. The resulting dataset is passed
#' to a {mod_compare03x} submodule for different calculations dependantant on the
#' option choosen in the {mod_aim01} module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param parameter a single character value with the name of the parameter for
#' subsetting the input dataset.
#' @return The following {shiny} widgets:
#' \itemize{
#'  \item{parameter}{a selectize input box with the name of the parameter.}
#'  \item{save}{an action button for saving the results. When results are saved, it is not shown.}
#'  \item{delete}{an action button for deleting the results. When results are not saved, it is not shown.}
#'  \item{nextbtn}{an action button visible when some results have been saved.}
#' }
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib layout_sidebar navset_hidden nav_panel
mod_compare03_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_sidebar(
    sidebar = list(

      ## sidebar
      # select the parameter
      selectizeInput(
        ns("parameter"),
        label = "Analita",
        selected = NULL,
        choices = NULL,
        multiple = FALSE,
        options = list(maxItems = 1)
      ),

      # different controls for the different data options ----
      bslib::navset_hidden(
        id = ns("ctrls"),

        bslib::nav_panel("2samples",
          mod_compare031_2samples_inputs_ui(ns("2samples"))),

        bslib::nav_panel("2samples_par",
          mod_compare032_2samples_par_inputs_ui(ns("2samples_par"))
        ),

        bslib::nav_panel("1sample_mu",
          mod_compare033_1sample_mu_inputs_ui(ns("1sample_mu"))),

        bslib::nav_panel("1sample_sigma",
          mod_compare034_1sample_sigma_inputs_ui(ns("1sample_sigma"))
        ),

        bslib::nav_panel("2values_unc",
          mod_compare035_2values_unc_inputs_ui(ns("2values_unc")))

      ),


      # save and delete buttons ----
      bslib::navset_hidden(
        id = ns("savedel"),


        bslib::nav_panel("cantsave"),
        # show the save button when data is not saved
        bslib::nav_panel("save",
                br(),
                 # 5. click on the save button
                 actionButton(
                   ns("save"),
                   "Salva",
                   icon = icon("floppy-disk")
                 )),

        # show the delete button when data has been saved
        bslib::nav_panel("delete",
                 br(),
                 # 6. click on the delete buttons (if you spot a mistake)
                 actionButton(
                   ns("delete"),
                   "Cancella",
                   icon = icon("eraser")
                 ))

      ),


      # showing a next button when something has been saved ----
      bslib::navset_hidden(
        id = ns("nextpanel"),

        bslib::nav_panel("not_saved",
                hr()
                ),

        bslib::nav_panel("saved",
                hr(),

                 actionButton(
                   ns("nextbtn"),
                   label = "Avanti",
                   icon = icon("circle-right")
                   ))
      )

    ),


      ## Main panel
      # different outputs for the different data options
      bslib::navset_hidden(
        id = ns("outputs"),

        bslib::nav_panel("2samples",
          mod_compare031_2samples_output_ui(ns("2samples"))),

        bslib::nav_panel("2samples_par",
          mod_compare032_2samples_par_output_ui(ns("2samples_par"))),

        bslib::nav_panel("1sample_mu",
          mod_compare033_1sample_mu_output_ui(ns("1sample_mu"))),

        bslib::nav_panel("1sample_sigma",
          mod_compare034_1sample_sigma_output_ui(ns("1sample_sigma"))),

        bslib::nav_panel("2values_unc",
          mod_compare035_2values_unc_output_ui(ns("2values_unc")))

      )
    )
  )
}

#' compare Server Function
#'
#' @description A shiny Module for basic two-sample hypothesis testing.
#' The module subsets a {data.table}, loaded by the {mod_loadfile02} module of the
#' {SIconfronta} package, by a single parameter. The resulting dataset is passed
#' to a {mod_compare03x} submodule for different calculations dependantant on the
#' option choosen in the {mod_aim01} module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param parameter a single character value with the name of the parameter for
#' subsetting the input dataset.
#' @param r a {reactiveValues} storing the results produced by the {mod_aim01}
#' and {mod_loadfile02} modules.
#' In \code{r$aim01$aim} the option chosen in the first module is stored, whereas
#' in \code{r$loadfile02$parlist} the list of the parameters extracted from the
#' loaded file is stored.
#' @return a {reactiveValues} with the following items stored in
#' \code{r$compare03$'selected parameter'} for each selected parameter:
#'    \itemize{
#'      \item{parameter}{the selected parameter;}
#'      \item{udm}{the unit of measurement;}
#'      \item{alternative}{the alternative hypothesis for the tests;}
#'      \item{significance}{the level of significance for the tests;}
#'      \item{data}{the subsetted dataset with a flag for removed or not removed values;}
#'      \item{summary}{a summary table;}
#'      \item{normality}{a Markdown formatted string with the results for the normality test.}
#'      \item{outliers}{a Markdown formatted string with the results for the outliers test.}
#'      \item{ttest}{a Markdown formatted string with the results for the t-test.}
#'      \item{ftest}{a Markdown formatted string with the results for the F-test.}
#'    }
#'  Additionally the selected parameter is stored in \code{r$compare03$myparameter}
#'  and a flag for saved data is stored in \code{r$compare03$saved_flag}.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
mod_compare03_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    r$compare03 <- reactiveValues()

    # updating the UI ----
    # updating the controls in the sidebar
    observeEvent(r$aim01$aim, {
      req(length(r$aim01$aim) == 1)

      updateTabsetPanel(inputId = "ctrls", selected = r$aim01$aim)
    })

    # updating the output in the mainpanel
    observeEvent(r$aim01$aim, {
      req(length(r$aim01$aim) == 1)

     updateTabsetPanel(inputId = "outputs", selected = r$aim01$aim)
    })

    observeEvent(r$loadfile02$parlist, {
      req(levels(r$loadfile02$parlist) != 0)

      parchoices <- c("", levels(r$loadfile02$parlist))
      updateSelectizeInput(session,
                           "parameter",
                           selected = "",
                           choices = parchoices
                           )
    })

    # storing the selected parameter to the r reactiveValues
    r$compare03 <- reactiveValues()

    observeEvent(input$parameter, {
      req(input$parameter != "")

      r$compare03$myparameter <- input$parameter
      r$compare03[[input$parameter]]$saved <- ifelse(is.null(r$compare03[[input$parameter]]$saved),
                                                     FALSE,
                                                     r$compare03[[input$parameter]]$saved)
    })

    # passing the r reactiveValues to different modules depending on the aim option ----
    to_mod_compare03x  <- reactive({
      switch (
        r$aim01$aim,
        "2samples" = mod_compare031_2samples_server("2samples", r),
        "2samples_par" = mod_compare032_2samples_par_server("2samples_par", r),
        "1sample_mu" = mod_compare033_1sample_mu_server("1sample_mu", r),
        "1sample_sigma" = mod_compare034_1sample_sigma_server("1sample_sigma", r),
        "2values_unc" = mod_compare035_2values_unc_server("2values_unc", r)
      )

    })

    # getting a ggplot2 version of the boxplot for the rmarkdown report ----
    myggbox <- reactive({
      switch (r$aim01$aim,
              "2samples" = ggboxplot_2samples(data = r$compare03x$data,
                                              group = r$loadfile02$groupvar,
                                              response = r$loadfile02$responsevar,
                                              udm = r$compare03x$udm),

              "2samples_par" = ggboxplot_2samples_par(data = r$compare03x$data,
                                                      group = r$loadfile02$groupvar,
                                                      response = r$loadfile02$responsevar,
                                                      udm = r$compare03x$udm,
                                                      group2 = r$compare03x$label2,
                                                      dfsummary = r$compare03x$ggsummary),

              "1sample_mu" = ggboxplot_1sample_mu(data = r$compare03x$data,
                                                  group = r$loadfile02$groupvar,
                                                  response = r$loadfile02$responsevar,
                                                  reflabel = r$compare03x$label2,
                                                  reference = r$compare03x$mean2,
                                                  udm = r$compare03x$udm),

              "1sample_sigma" = ggboxplot_1sample_sigma(data = r$compare03x$data,
                                                        group = r$loadfile02$groupvar,
                                                        response = r$loadfile02$responsevar,
                                                        reflabel = r$compare03x$label2,
                                                        reference = r$compare03x$sd2,
                                                        udm = r$compare03x$udm),

              "2values_unc" =  ggboxplot_2values_unc(data = r$compare03x$data,
                                                     group = r$loadfile02$groupvar,
                                                     response = r$loadfile02$responsevar,
                                                     uncertainty = r$loadfile02$uncertaintyvar,
                                                     udm = r$compare03x$udm)
      )
    })

    observeEvent(r$compare03$myparameter, {
      to_mod_compare03x()
    })

    # when save is clicked the results are stored into r ----
    observeEvent(input$save, {

      r$compare03[[input$parameter]]$parameter <- r$compare03x$parameter
      r$compare03[[input$parameter]]$udm <- r$compare03x$udm
      r$compare03[[input$parameter]]$alternative <- r$compare03x$alternative
      r$compare03[[input$parameter]]$significance <- r$compare03x$significance
      r$compare03[[input$parameter]]$label2 <- r$compare03x$label2
      r$compare03[[input$parameter]]$mean2 <- r$compare03x$mean2
      r$compare03[[input$parameter]]$sd2 <- r$compare03x$sd2
      r$compare03[[input$parameter]]$n2 <- r$compare03x$n2
      r$compare03[[input$parameter]]$data <- r$compare03x$data
      r$compare03[[input$parameter]]$summary <- r$compare03x$summary
      r$compare03[[input$parameter]]$normality_html <- r$compare03x$normality
      r$compare03[[input$parameter]]$outliers_html <- r$compare03x$outliers
      r$compare03[[input$parameter]]$ttest_html <- r$compare03x$ttest
      r$compare03[[input$parameter]]$ftest_html <- r$compare03x$ftest
      r$compare03[[input$parameter]]$normality <- r$compare03x$normality |> htmltormarkdown()
      r$compare03[[input$parameter]]$outliers <- r$compare03x$outliers |> htmltormarkdown()
      r$compare03[[input$parameter]]$ttest <- r$compare03x$ttest |> htmltormarkdown()
      r$compare03[[input$parameter]]$ftest <- r$compare03x$ftest |> htmltormarkdown()
      r$compare03[[input$parameter]]$plotlyboxplot <- r$compare03x$plotlyboxplot
      r$compare03[[input$parameter]]$boxplot <- myggbox()
      r$compare03[[input$parameter]]$saved <- TRUE


      showModal(
        modalDialog(
          title = "Hai salvato i risultati",
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
    })

    # when delete is clicked the results is removed from r ----
    observeEvent(input$delete, {

      r$compare03[[input$parameter]] <- NULL
      r$compare03[[input$parameter]]$saved <- FALSE
    })

    # updating the save and delete panels ----
    savedel_flag <- reactive({
      req(input$parameter)

      ifelse(r$compare03x$click != 1, "cantsave",
      ifelse(r$compare03[[input$parameter]]$saved |> isTRUE(), "delete", "save")
      )
    })

    observeEvent(savedel_flag(), {
      updateTabsetPanel(inputId = "savedel", selected = savedel_flag())
    })

    # updating the next panel ----
    somethingsaved <- reactive({
      req(input$parameter)

      mylist <- reactiveValuesToList(r$compare03)

      lapply(mylist, function(x) x["saved"] == "TRUE") |>
         unlist() |>
         sum(na.rm = TRUE) |>
         (\(x) ifelse(x >= 1, "saved", "not_saved"))()
    })

    observeEvent(somethingsaved(), {
      updateTabsetPanel(inputId = "nextpanel", selected = somethingsaved())
    })


    observeEvent(input$nextbtn, {
      showModal(
        modalDialog(
          title = "Non potrai tornare indietro",
          "Hai salvato i risultati per tutti i parametri di interesse?",
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("yesbtn"), "S\u00EC"),
            modalButton("No")
          )
        )
      )
    })

    observeEvent(input$yesbtn, {
      removeModal()

      r$compare03$saved_flag <- input$nextbtn
    })


  })
}
