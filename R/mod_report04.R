#' Report04 UI Function
#'
#' @description A shiny module for simple reporting by {pdf} Rmarkdown.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return a text input (\code{expaim}), several area and text input (,
#' \code{method}, \code{instrument}, \code{samples},
#' \code{workers}, \code{description}, \code{discussion}),
#'  a group of check boxes (\code{content}) and two download button
#'  (\code{makereport} and \code{getword}).
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib card card_header card_body layout_columns layout_column_wrap
mod_report04_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::layout_columns(
      col_widths = c(7, 5),
      fill = FALSE,

    bslib::card(
      bslib::card_header(icon("hand-point-down"), "Aggiungi qualche informazione"),
      bslib::card_body(
        textAreaInput(ns("expaim"), label = "Scopo dell'esperimento",
                      rows = 5, width = "100%"),

        bslib::layout_columns(
          col_widths = c(6, 6),
          fill = FALSE,

          textInput(ns("method"), label = "Identificativo del metodo di misura",
                    width = "100%"),

          textInput(ns("instrument"), label = "Identificativo dello strumento di misura",
                    width = "100%"),

          textAreaInput(
            ns("samples"),
            label = "Campioni sottoposti a misura",
            rows = 3,
            width = "100%"
          ),

          textAreaInput(
            ns("workers"),
            label = "Operatori coinvolti nell'esperimento",
            rows = 3,
            width = "100%"
          )

        ),
        textAreaInput(ns("description"), label = "Descrizione dell'esperimento",
                      rows = 10, width = "100%"),
        textAreaInput(ns("discussion"), label = "Interpretazione dei risultati",
                      rows = 10, width = "100%")
      )
    ),

    bslib::layout_column_wrap(
      width = 1,
      heights_equal = "row",

    bslib::card(
      bslib::card_header(icon("hand-point-down"), "Seleziona cosa salvare"),
      bslib::card_body(

        checkboxGroupInput(ns("content"),
                       label = h4("Test e prestazioni da includere nel report"),
                       width = "80%",
                       choices = "",
                       selected = "")
        )
      ),

    bslib::card(
      bslib::card_header(icon("lightbulb"), "Suggerimento"),
      bslib::card_body(
        shiny::tags$p(
        "Una volta cliccato il tasto 'Crea un report archiviabile',
        non chiudere o ricaricare la pagina finch\u00E9 non troverai
        nella tua cartella Download un file pdf con il nome
        'performances-report_' seguito dalla data di oggi."
        ),

        shiny::tags$p(
        "Analogamente, cliccando sul tasto 'Crea un riepilogo modificabile',
        troverai nella tua cartella Download un file docx con il nome
        'word_report-' seguito dalla data di oggi."),

        shiny::tags$p(
        "A seconda di quanti parametri hai salvato, potrebbero volerci
        fino a un massimo di dieci minuti,
        anche se tipicamente ne bastano un paio."
        )
      )
    )
    )
    ),

    tags$div(style = "padding-bottom: 15px",
      downloadButton(ns("makereport"), label = "Crea un report archiviabile",
                     width = '25%'),
      downloadButton(ns("getword"), label = "Crea un riepilogo modificabile",
                     width = '25%')
    )

  )
}

#' makereport Server Functions
#'
#' @description A shiny module for simple reporting by {pdf} Rmarkdown.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param inputreport a list of saved results to be passed to the {rmd}
#'  report template. Additionally, the module uses a report template named
#'  {comparison_report.Rmd} and a pdf logo named {"logoarpal.pdf"} located in
#'  the {"SIprecisa/inst"} folder.
#' @return a {pdf} report compiled following the {comparison_report.Rmd} template.
#'
#' @noRd
#'
#' @import data.table
#' @importFrom sessioninfo session_info
mod_report04_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # dinamically update the checkboxgroupinput ----
    observeEvent(r$estimate03$saved_flag, {
      mylist <- reactiveValuesToList(r$estimate03)
      mylist <- mylist[names(mylist) %notin% c("myparameter", "saved_flag")]
      # removing not saved results
      mylist <- sapply(mylist, function(x) isTRUE(x$saved)) |>
        (\(x) mylist[x])()

      normality <- sapply(mylist, function(x) ! is.na(x$normality)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "normality", NA))()
      names(normality) <- "Normalit\u00E0 e outliers"

      trueness <- sapply(mylist, function(x) ! is.na(x$trueness)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "trueness", NA))()
      names(trueness) <- "Giustezza"

      precision <- sapply(mylist, function(x) ! is.na(x$precision)) |>
        sum() |>
        (\(x) ifelse(x >= 1, "precision", NA))()
      names(precision) <- "Precisione"

      mychoices <- c(normality, trueness, precision)
      mychoices <- mychoices[!is.na(mychoices)]

      freezeReactiveValue(input, "content")
      updateCheckboxGroupInput(session,
                               "content",
                               choices = mychoices,
                               selected = mychoices)
    })

   sysdate <- ifelse(isTRUE(getOption("shiny.testmode")), "testdate",
                     Sys.Date() |> as.character())

    r$report04 <- reactiveValues(info = sessioninfo::session_info())

    output$makereport <- downloadHandler(
      filename = function() {
        paste0("performances_report-", sysdate, ".pdf")
      },
      content = function(file) {
        withProgress(message = "Sto scrivendo il report...", {
        # The report template is copied in a temporary directory to prevent
        # user permission issues
        reportpath <- system.file("rmd", "performances_report.Rmd",
                                  package = "SIprecisa")
        logopath <- system.file("rmd", "logo.pdf",
                                package = "SIprecisa")

        tempReport <- tempfile(fileext = ".Rmd")
        tempLogo <- tempfile(fileext = ".pdf")
        file.copy(reportpath, tempReport, overwrite = TRUE)
        file.copy(logopath, tempLogo, overwrite = TRUE)

        r$report04$logo <- logopath
        r$report04$expaim <- input$expaim
        r$report04$samples <- input$samples
        r$report04$workers <- input$workers
        r$report04$method <- input$method
        r$report04$instrument <- input$instrument
        r$report04$description <- input$description
        r$report04$discussion <- input$discussion
        r$report04$content <- input$content
        r$report04$testmode <- isTRUE(getOption("shiny.testmode"))

        # input parameters for the rmd file ----
        params <- isolate(lapply(r, reactiveValuesToList))

        n_par <- length(params$estimate03) - 2
        for (i in n_par) {
          Sys.sleep(1)
          incProgress(1 / n_par)
        }

        # the report is compiled in a separate R environment
        render_report(input = tempReport,
                      output = file,
                      params = params)

        })
      }
    )

    output$getword <- downloadHandler(
      filename = function() {
        paste0("word_report-", sysdate, ".docx")
      },
      content = function(file) {
        withProgress(message = "Sto scrivendo il report...", {
          # The report template is copied in a temporary directory to prevent
          # user permission issues
          wordpath <- system.file("rmd", "word_report.Rmd",
                                  package = "SIprecisa")

          wordReport <- tempfile(fileext = ".Rmd")
          file.copy(wordpath, wordReport, overwrite = TRUE)

          r$report04$expaim <- input$expaim
          r$report04$samples <- input$samples
          r$report04$workers <- input$workers
          r$report04$method <- input$method
          r$report04$instrument <- input$instrument
          r$report04$description <- input$description
          r$report04$discussion <- input$discussion
          r$report04$content <- input$content
          r$report04$testmode <- isTRUE(getOption("shiny.testmode"))

          # input parameters for the rmd file ----
          params <- isolate(lapply(r, reactiveValuesToList))

          n_par <- length(params$estimate03) - 2
          for (i in n_par) {
            Sys.sleep(1)
            incProgress(1 / n_par)
          }

          # the report is compiled in a separate R environment
          render_report(input = wordReport,
                        output = file,
                        params = params)

        })
      }
    )

  })
}
