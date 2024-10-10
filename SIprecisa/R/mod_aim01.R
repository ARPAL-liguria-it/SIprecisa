#' 01_aim UI Function
#'
#' @description A shiny Module for selecting which elaboration will be
#' performed by the {SIprecisa} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r a {reactiveValues} list.
#' @return a list of radio buttons with the elaborations and an action button
#' and an action button for saving the results.
#'
#' @noRd
#'
#' @import shiny
#' @import markdown
#' @importFrom bslib card card_header card_footer card_body navset_hidden nav_panel layout_columns
mod_aim01_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
    col_widths = c(7, 5),
    fill = FALSE,

    # selecting the aim of the comparison ----
    bslib::card(
      bslib::card_header(icon("hand-point-down"), "Scegli il tuo obiettivo"),
      bslib::card_body(
        class = "gap-2 container",
        radioButtons(
          ns("aim"),
          label = NULL,
          choices = c(
            "Stima di ripetibilit\u00E0 e recupero dalla stessa serie di misure" = "riprec",
            "Stima della ripetibilit\u00E0 da una serie di misure in doppio" = "rip",
            "Stima del recupero da un valore singolo" = "recuno"
          ),
          width = "100%"
        ),

        tags$div(actionButton(
          ns("nextbtn"),
          label = "Avanti",
          icon = icon("circle-right")
        ))
      ),

      bslib::card_footer(
        shiny::tags$div(
          shiny::tags$span(style = "font-size:smaller",
            paste0("SI precisa ", get_gh_version("andreabz", "SIprecisa"),
                    ", validato al momento del rilascio ")),
                    shiny::tags$a(href = "https://github.com/andreabz/SIprecisa/actions/workflows/test-coverage.yaml",
                      shiny::tags$img(src = "https://github.com/andreabz/SIprecisa/actions/workflows/test-coverage.yaml/badge.svg",
                                      alt = "esito della validazione")
                                  ))
        )
    ),

    # conditional tabset with examples and instructions for the different options ----
    bslib::navset_hidden(

      id = ns("example"),

      bslib::nav_panel("riprec",
                       help_accordion(
                         todotitle = "Cosa ti serve",
                         tipstitle = "Suggerimento",
                         togettitle = "Cosa otterrai",
                         todofile = "help_aim01_riprec_todo.Rmd",
                         tipsfile = "help_aim01_tips.Rmd",
                         togetfile = "help_aim01_riprec_toget.Rmd"
                       )
      ),

      bslib::nav_panel("rip",
                       help_accordion(
                         todotitle = "Cosa ti serve",
                         tipstitle = "Suggerimento",
                         togettitle = "Cosa otterrai",
                         todofile = "help_aim01_rip_todo.Rmd",
                         tipsfile = "help_aim01_tips.Rmd",
                         togetfile = "help_aim01_rip_toget.Rmd"
                       )
      ),

      bslib::nav_panel("recuno",
                       withMathJax(
                         help_accordion(
                           todotitle = "Cosa ti serve",
                           tipstitle = "Suggerimento",
                           togettitle = "Cosa otterrai",
                           todofile = "help_aim01_recuno_todo.Rmd",
                           tipsfile = "help_aim01_tips.Rmd",
                           togetfile = "help_aim01_recuno_toget.Rmd"
                         )
                       ))
    )

  ))
}

#' 01_aim Server Functions
#'
#' @description A shiny Module for selecting which elaboration will be
#' performed by the {SIprecisa} shinyAPP.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return a reactiveValues with the option chosen in UI stored into \code{r$aim01$aim}.
#'  Possible values are \code{"riprec"}, \code{"rip"}, \code{"rec"} and \code{"recuno"}.
#'
#' @noRd
#'
#' @import shiny
mod_aim01_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    # updating examples and instructions depending on the option selected ----
    observeEvent(input$aim, {
      updateTabsetPanel(inputId = "example", selected = input$aim)
    })


    # adding the selected option to the r list ----
    r$aim01 <- reactiveValues()

    observeEvent(input$nextbtn, {
      showModal(
        modalDialog(
          title = "Non potrai tornare indietro",
          "Confermi la tua scelta?",
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

      r$aim01$aim <- input$aim
      r$aim01$version <- get_gh_version("andreabz", "SIprecisa")
    })

  })
}

## To be copied in the UI
# mod_aim01_ui("aim01_1")

## To be copied in the server
# mod_aim01_server("aim01_1")
