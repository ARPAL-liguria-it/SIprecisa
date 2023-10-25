#' estimate UI Function:
#'
#' @description A shiny Module for estimating performance parameters of
#' chemical analytical methods.
#' The module subsets a {data.table}, loaded by the {mod_loadfile02} module of the
#' {SIprecisa} package, by a single parameter. The resulting dataset is passed
#' to a {mod_estimate03x} submodule for different calculations dependantant on the
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
mod_estimate03_ui <- function(id) {
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

        bslib::nav_panel("riprec",
          mod_estimate031_riprec_inputs_ui(ns("riprec"))),

        bslib::nav_panel("rip",
          mod_estimate032_rip_inputs_ui(ns("rip"))
         )#,
      #
      #   bslib::nav_panel("rec",
      #     mod_estimate033_rec_inputs_ui(ns("rec"))),
      #
      #   bslib::nav_panel("recuno",
      #     mod_estimate035_recuno_inputs_ui(ns("recuno")))
      #
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

        bslib::nav_panel("riprec",
          mod_estimate031_riprec_output_ui(ns("riprec"))),

        bslib::nav_panel("rip",
          mod_estimate032_rip_output_ui(ns("rip")))#,
        #
        # bslib::nav_panel("rec",
        #   mod_estimate033_rec_output_ui(ns("rec"))),
        #
        # bslib::nav_panel("recuno",
        #   mod_estimate035_recuno_output_ui(ns("recuno")))

      )
    )
  )
}

#' estimate Server Function
#'
#' @description A shiny Module for estimating performance parameters of
#' chemical analytical methods.
#' The module subsets a {data.table}, loaded by the {mod_loadfile02} module of the
#' {SIprecisa} package, by a single parameter. The resulting dataset is passed
#' to a {mod_estimate03x} submodule for different calculations dependantant on the
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
#' \code{r$estimate03$'selected parameter'} for each selected parameter:
#'    \itemize{
#'      \item{parameter}{the selected parameter;}
#'      \item{udm}{the unit of measurement;}
#'      \item{significance}{the level of significance for the tests;}
#'      \item{data}{the subsetted dataset with a flag for removed or not removed values;}
#'      \item{summary}{a summary table;}
#'      \item{normality}{a Markdown formatted string with the results for the normality test.}
#'      \item{outliers}{a Markdown formatted string with the results for the outliers test.}
#'      \item{ttest}{a Markdown formatted string with the results for the t-test.}
#'    }
#'  Additionally the selected parameter is stored in \code{r$estimate03$myparameter}
#'  and a flag for saved data is stored in \code{r$estimate03$saved_flag}.
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
mod_estimate03_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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

      freezeReactiveValue(input, "parameter")
      updateSelectizeInput(session,
                           "parameter",
                           selected = "",
                           choices = parchoices
                           )
    })

    # storing the selected parameter to the r reactiveValues
    r$estimate03 <- reactiveValues()

    observeEvent(input$parameter, {
      req(input$parameter != "")

      r$estimate03$myparameter <- input$parameter
      r$estimate03[[input$parameter]]$saved <- ifelse(is.null(r$estimate03[[input$parameter]]$saved),
                                                     FALSE,
                                                     r$estimate03[[input$parameter]]$saved)
    })

    # passing the r reactiveValues to different modules depending on the aim option ----
    to_mod_estimate03x  <- reactive({
      switch (
        r$aim01$aim,
        "riprec" = mod_estimate031_riprec_server("riprec", r),
        "rip" = mod_estimate032_rip_server("rip", r)#,
        # "rec" = mod_estimate033_rec_server("rec", r),
        # "recuno" = mod_estimate035_recuno_server("recuno", r)
      )

    })

    # getting a ggplot2 version of the boxplot for the rmarkdown report ----
    myggbox <- reactive({
      switch (r$aim01$aim,
              "riprec" = ggboxplot_riprec(data = r$estimate03x$data,
                                          response = r$loadfile02$responsevar,
                                          refvalue = r$estimate03x$refvalue,
                                          refuncertainty = r$estimate03x$refuncertainty,
                                          conflevel = r$estimate03x$significance |> as.numeric(),
                                          udm = r$estimate03x$udm),

              "rip" = ggboxplot_rip(data = r$estimate03x$data,
                                    response = r$loadfile02$responsevar,
                                    udm = r$estimate03x$udm)#,
              #
              # "rec" = ggboxplot_riprec(data = r$estimate03x$data,
              #                          response = r$loadfile02$responsevar,
              #                          refvalue = r$estimate03x$refval,
              #                          refuncertainty = r$estimate03x$refuncertainty,
              #                          udm = r$estimate03x$udm),
              #
              # "recuno" = ggboxplot_recuno(data = r$estimate03x$data,
              #                             response = r$loadfile02$responsevar,
              #                             uncertainty = r$loadfile02$uncertaintyvar,
              #                             refvalue = r$estimate03x$refval,
              #                             refuncertainty = r$estimate03x$refuncertainty,
              #                             udm = r$estimate03x$udm)
      )
    })

    observeEvent(r$estimate03$myparameter, {
      to_mod_estimate03x()
    })

    # when save is clicked the results are stored into r ----
    observeEvent(input$save, {

      r$estimate03[[input$parameter]]$parameter <- r$estimate03x$parameter
      r$estimate03[[input$parameter]]$udm <- r$estimate03x$udm
      r$estimate03[[input$parameter]]$significance <- r$estimate03x$significance
      r$estimate03[[input$parameter]]$refvalue <- r$estimate03x$refvalue
      r$estimate03[[input$parameter]]$refuncertainty <- r$estimate03x$refuncertainty
      r$estimate03[[input$parameter]]$data <- r$estimate03x$data
      r$estimate03[[input$parameter]]$summary <- r$estimate03x$summary
      r$estimate03[[input$parameter]]$normality_html <- r$estimate03x$normality
      r$estimate03[[input$parameter]]$outliers_html <- r$estimate03x$outliers
      r$estimate03[[input$parameter]]$ttest_html <- r$estimate03x$ttest
      r$estimate03[[input$parameter]]$trueness_html <- r$estimate03x$trueness
      r$estimate03[[input$parameter]]$precision_html <- r$estimate03x$precision
      r$estimate03[[input$parameter]]$normality <- r$estimate03x$normality |> htmltormarkdown()
      r$estimate03[[input$parameter]]$outliers <- r$estimate03x$outliers |> htmltormarkdown()
      r$estimate03[[input$parameter]]$ttest <- r$estimate03x$ttest |> htmltormarkdown()
      r$estimate03[[input$parameter]]$trueness <- r$estimate03x$trueness |> htmltormarkdown()
      r$estimate03[[input$parameter]]$precision <- r$estimate03x$precision |> htmltormarkdown()
      r$estimate03[[input$parameter]]$plotlyboxplot <- r$estimate03x$plotlyboxplot
      r$estimate03[[input$parameter]]$plotlyconfint <- r$estimate03x$plotlyconfint
      r$estimate03[[input$parameter]]$boxplot <- myggbox()
      r$estimate03[[input$parameter]]$saved <- TRUE

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

      r$estimate03[[input$parameter]] <- NULL
      r$estimate03[[input$parameter]]$saved <- FALSE
    })

    # updating the save and delete panels ----
    savedel_flag <- reactive({
      req(input$parameter)

      ifelse(r$estimate03x$click != 1, "cantsave",
      ifelse(r$estimate03[[input$parameter]]$saved |> isTRUE(), "delete", "save")
      )
    })

    observeEvent(savedel_flag(), {
      updateTabsetPanel(inputId = "savedel", selected = savedel_flag())
    })

    # updating the next panel ----
    somethingsaved <- reactive({
      req(input$parameter)

      mylist <- reactiveValuesToList(r$estimate03)

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

      r$estimate03$saved_flag <- input$nextbtn
    })


  })
}
