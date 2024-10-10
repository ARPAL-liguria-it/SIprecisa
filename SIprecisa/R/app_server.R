#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinyjs enable disable
app_server <- function(input, output, session) {
  options(scipen = 999)
  # disabling all tabs except aim and readme when the app starts ----
  shinyjs::disable(selector = "#navbar li a[data-value=data]")
  shinyjs::disable(selector = "#navbar li a[data-value=estimate]")
  shinyjs::disable(selector = "#navbar li a[data-value=report]")

  r <- reactiveValues()
  mod_aim01_server("scopo", r)
  mod_loadfile02_server("dati", r)
  mod_estimate03_server("stima", r)
  mod_report04_server("report", r)


  # enable and disable tabs when clicking on the next buttons ----
  observeEvent(input$`scopo-yesbtn`, {
    shinyjs::enable(selector = "#navbar li a[data-value=data]")
    shinyjs::disable(selector = "#navbar li a[data-value=aim]")
    updateNavbarPage(session, "navbar", "data")
  })

  observeEvent(input$`dati-yesbtn`, {
    shinyjs::enable(selector = "#navbar li a[data-value=estimate]")
    shinyjs::disable(selector = "#navbar li a[data-value=data]")
    updateNavbarPage(session, "navbar", "estimate")
  })

  observeEvent(input$`stima-yesbtn`, {
    shinyjs::enable(selector = "#navbar li a[data-value=report]")
    shinyjs::disable(selector = "#navbar li a[data-value=estimate]")
    updateNavbarPage(session, "navbar", "report")
  })

}
