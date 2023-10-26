#' loadfile UI Function
#'
#' @description A shiny Module for loading csv dataset.
#'   The dataset is provided by clicking on the corresponding UI folder icon and
#'   by browsing the the desired file location.
#'   The file to be imported is required to have:
#'   \itemize{
#'    \item a .csv extension;
#'    \item two \code{character} columns;
#'    \item one or two \code{numeric} variable columns,
#'    depending on the \code{r$aim01$aim} value.
#'   }
#'   Column headers and dot "." as decimal separator are mandatory.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @return A page with the following {shiny} widget:
#'  \itemize{
#'    \item{file} a fileInput widget for uploading the file;
#'    \item{parvar} a selectizeInput widget for selecting the column name of the imported dataset hosting the parameter labels;
#'    \item{responsevar} a selectizeInput widget for selecting the column name of the imported dataset hosting the numerical response values;
#'    \item{uncertaintyvar} a selectizeInput widget for selecting the column name of the imported dataset hosting the numerical
#'    extended uncertainty values;
#'    \item{nextbtn} an actionButton widget for saving the imported data.
#'  }
#'
#' @noRd
#'
#' @import shiny
#' @importFrom bslib layout_sidebar navset_hidden nav_panel card card_header card_body
mod_loadfile02_ui <- function(id) {
  ns <- NS(id)

  tagList(

    bslib::layout_sidebar(
    sidebar = list(

      ## sidebar
      # control for choosing the file to be uploaded
      fileInput(
        ns("file"),
        label = "CSV file",
        accept = ".csv",
        multiple = FALSE,
        buttonLabel = icon("folder"),
        placeholder = "vuoto"
      ),


      # Conditional panel for assigning the column names to their role
      bslib::navset_hidden(
        id = ns("columnnames"),

        bslib::nav_panel(""),

        bslib::nav_panel(
          "dataloaded",
          ## 1. Select the variable for the parameter
          selectizeInput(
            ns("parvar"),
            label = "Analiti",
            selected = NULL,
            choices = NULL,
            multiple = FALSE,
            options = list(maxItems = 1)
          ),
          ## 2. Select the variable for response
          selectizeInput(
            ns("responsevar"),
            label = "Risposte",
            selected = NULL,
            choices = NULL,
            multiple = FALSE,
            options = list(maxItems = 1)
          )
        )
      ),

      bslib::navset_hidden(
        id = ns("paired"),

        bslib::nav_panel("not_2measures"),

        bslib::nav_panel(
          "2measures",
          selectizeInput(
            ns("secondresponsevar"),
            label = "Seconda serie di risposte",
            selected = NULL,
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 1)
          )
        )
      ),


      bslib::navset_hidden(
        id = ns("ext_unc"),

        bslib::nav_panel("without_unc"),

        bslib::nav_panel(
          "with_unc",
          selectizeInput(
            ns("uncertaintyvar"),
            label = "Incertezza estesa",
            selected = NULL,
            choices = NULL,
            multiple = TRUE,
            options = list(maxItems = 1)
          )
        )
      ),


      bslib::navset_hidden(
        id = ns("next"),

        bslib::nav_panel(""),

        bslib::nav_panel("dataloaded",
                         hr(),
                         actionButton(
                           ns("nextbtn"),
                           label = "Avanti",
                           icon = icon("circle-right")
                         ))
      )

    ),

    ## main panel
    bslib::navset_hidden(
      id = ns("data"),

      bslib::nav_panel("",
                       bslib::card(
                         bslib::card_header(icon("hammer"), "Cosa devi fare"),
                         bslib::card_body(htmlOutput(ns("waiting")))
                         )
                       ),


      bslib::nav_panel("dataloaded",
                      bslib::card(
                        bslib::card_header(icon("vials"), "Dati caricati"),
                        bslib::card_body(
                          DT::DTOutput(ns("datatable")))
                       )
                      )
    )
  ))
}

#' loadfile Server Functions
#'
#' @description A shiny Module for loading csv dataset.
#'   The dataset is provided by clicking on the corresponding UI folder icon and
#'   by browsing the the desired file location.
#'   The file to be imported is required to have:
#'   \itemize{
#'    \item a .csv extension;
#'    \item two \code{character} columns;
#'    \item one or two \code{numeric} variable columns,
#'    depending on the \code{r$aim01$aim} value.
#'   }
#'   Column headers and dot "." as decimal separator are mandatory.
#'
#'   When \code{stringAsFactors = TRUE} character columns are converted to \code{factor}.
#'
#' @details Data are imported by \code{data.table::fread} which takes care of guessing
#'  the field separator and handles the text delimiter, such as "".
#'  Decimal separator is the dot "." and numeric values with comma "," as decimal
#'  separator will be treated as characters.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param r reactiveValues stores the information saved by other modules.
#' In \code{r$aim01$aim} is stored the option selected in the {mod_aim01} module.
#' @return A list of values appended as reactiveValues into \code{r$loadfile02}:
#'  \itemize{
#'    \item{data} is the imported data.frame;
#'    \item{parvar} is the data.frame column name in which parameters are stored;
#'    \item{parlist} is the list of parameters provided by the data.frame;
#'    \item{responsevar} is the data.frame column name in which the response numerical values are stored;
#'    \item{uncertaintyvar} is the data.frame column name in which the extended uncertainty numerical values are stored.
#'  }
#'
#' @noRd
#'
#' @import shiny
#' @import data.table
#' @import markdown
#' @importFrom DT datatable renderDT
#' @importFrom stats aggregate
mod_loadfile02_server <- function(id, r) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    is_length_aim_one <- reactive(length(r$aim01$aim) == 1)

    # instructions for different options ----
    msg_waiting <- reactive({
      req(is_length_aim_one())

      switch(
        r$aim01$aim,
        "riprec" = includeMarkdown(system.file("rmd", "help_loadfile02_riprec.Rmd", package = "SIprecisa")),
        "rip" = includeMarkdown(system.file("rmd", "help_loadfile02_rip.Rmd", package = "SIprecisa")),
        "recuno" = includeMarkdown(system.file("rmd", "help_loadfile02_recuno.Rmd", package = "SIprecisa"))
      )
    })

    output$waiting <- renderUI({
      msg_waiting()
    })


    # loading the data ----
    userFile <- reactive({
      # waiting for the file
      req(input$file$name)
      req(input$file$datapath)

      input$file
    })

    # printing the filename to the console ----
    observeEvent(userFile()$name, {
      msg <- sprintf("File %s was uploaded", userFile()$name)
      cat(msg, "\n")
    })

    # data are imported as a data.table with fread
    datafile <- reactive({
      data.table::fread(userFile()$datapath,
                        header = "auto",
                        stringsAsFactors = TRUE)
    })


    # validating the data ----
    ## required number of numerical columns
    reqsumnum <- reactive({
      req(is_length_aim_one())

      ifelse(r$aim01$aim %in% c("recuno", "rip"), 2, 1)
    })

    ## required minimum number of values
    reqminvalues <- reactive({
      req(is_length_aim_one())

      switch(
        r$aim01$aim,
        "riprec" = 6,
        "rip" = 8,
        "recuno" = 1
      )
    })

    ## required maximum number of values
    reqmaxvalues <- reactive({
      req(is_length_aim_one())

      switch(
        r$aim01$aim,
        "riprec" = 30,
        "rip" = 30,
        "recuno" = 1
      )
    })

    ## numeric variables
    numcol <- reactive({
      colnames(datafile())[sapply(datafile(), is.numeric)]
    })

    ## number of numeric variables
    sumnum <- reactive(length(numcol()))

    ## checking how many numerical columns are in the dataset
    numok <- reactive({
      shiny::validate(
        shiny::need(sumnum() == reqsumnum(),
             message = sprintf(
               "Hai fornito un dataset con %s colonn%s di numeri ma ne %s.",
               sumnum(),
               ifelse(sumnum() == 1, "a", "e"),
               ifelse(reqsumnum() == 1, "serve 1", "servono 2")
             ))
      )

      sumnum() == reqsumnum()
    })

    ## character variables
    charcol <- reactive({
      colnames(datafile())[sapply(datafile(), is.factor)]
    })

    ## number of character variables
    sumchar <- reactive(length(charcol()))

    ### checking how many character columns are in the dataset
    charok <- reactive({
      validate(
        need(sumchar() == 1,
             message = sprintf(
             "Hai fornito un dataset con %s colonn%s di testo ma ne serve 1.",
             sumchar(),
             ifelse(sumchar() == 1, "a", "e")
             ))
      )

      sumchar() == 1
    })

    ## max number of values for each parameter
    maxvalues <- reactive({
      req(input$parvar)
      req(input$responsevar)

      stats::aggregate(x = datafile()[[input$responsevar]] ,
                       by = list(datafile()[[input$parvar]]),
                       FUN = length)$x |> max()
    })

    ## min number of values for each parameter
    minvalues <- reactive({
      req(input$parvar)
      req(input$responsevar)

      stats::aggregate(x = datafile()[[input$responsevar]] ,
                       by = list(datafile()[[input$parvar]]),
                       FUN = length)$x |> min()
    })

    ### checking how many values for each parameters are in the dataset
    valuesok <- reactive({
      validate(
        need(maxvalues() <= reqmaxvalues(),
             message = sprintf(
               "Hai fornito un dataset con un massimo di %s valor%s per analita ma posso gestirne al massimo fino a %s.",
               maxvalues(),
               ifelse(maxvalues() == 1, "e", "i"),
               reqmaxvalues()
             )),
        need(minvalues() >= reqminvalues(),
             message = sprintf(
               "Hai fornito un dataset con un minimo di %s valor%s per analita ma non posso gestirne meno di %s.",
               minvalues(),
               ifelse(minvalues() == 1, "e", "i"),
               reqminvalues()
             ))
      )

      maxvalues() <= reqmaxvalues() & minvalues() >= reqminvalues()

    })


    ## checking that is all fine
    dataok <- reactive({
      charok() & numok() & valuesok()
    })


    # updating the UI with the new data ----
    ## trigger for the columnnames tabsetPanel
    isloaded <- reactive({
      req(is_length_aim_one())

      ifelse(isTRUE(dataok()), "dataloaded", "")
    })

    ## trigger for the ext_unc tabsetPanel
    isunc <- reactive({
      req(is_length_aim_one())

      ifelse(isTRUE(dataok()),
             ifelse(r$aim01$aim == "recuno", "with_unc", "without_unc"),
             "")
    })

    ## trigger for the paired tabsetPanel
    is2measures <- reactive({
      req(is_length_aim_one())

      ifelse(isTRUE(dataok()),
             ifelse(r$aim01$aim == "rip", "2measures", "not_2measures"),
             "")
    })

    ## updating columnnames tabsetPanel depending on the option selected
    observeEvent(isloaded(), {

      updateTabsetPanel(inputId = "columnnames", selected = isloaded() |> unname())
    })

    ## updating paired tabsetPanel depending on the option selected
    observeEvent(is2measures(), {

      updateTabsetPanel(inputId = "paired", selected = is2measures() |> unname())
    })

    ## updating ext_unc tabsetPanel depending on the option selected
    observeEvent(isunc(), {

      updateTabsetPanel(inputId = "ext_unc", selected = isunc() |> unname())
    })

    ## updating next tabsetPanel depending on the option selected
    observeEvent(isloaded(), {

      updateTabsetPanel(inputId = "next", selected = isloaded() |> unname())
    })

    ## trigger for the data tabsetPanel
    dataloaded <- reactive({

      ifelse(is.null(datafile()), "", "dataloaded")
    })

    ## updating data tabsetPanel depending if data has been loaded or not
    observeEvent(dataloaded(), {

      updateTabsetPanel(inputId = "data", selected = dataloaded() |> unname())
    })

    ## update input for variable selection
    observeEvent(charcol(), {
      req(isTRUE(charok()))

      freezeReactiveValue(input, "parvar")
      updateSelectizeInput(session,
                           "parvar",
                           selected = charcol()[1],
                           choices = charcol())
    })

    ## update input for response variable
    observeEvent(numcol(), {
      req(isTRUE(numok()))

      freezeReactiveValue(input, "responsevar")
      updateSelectizeInput(session,
                           "responsevar",
                           label = ifelse(r$aim01$aim == "rip", "Prima serie di risposte", "Risposte"),
                           selected = numcol()[1],
                           choices = numcol())
    })

    ## update input for secondresponse variable
    observeEvent(input$responsevar, {
      req(is2measures() == "2measures")

      freezeReactiveValue(input, "secondresponsevar")
      updateSelectizeInput(session,
                           "secondresponsevar",
                           selected = numcol()[2],
                           choices = numcol())
    })

    ## update input for uncertainty variable
    observeEvent(input$responsevar, {
      req(isunc() == "with_unc")

      freezeReactiveValue(input, "uncertaintyvar")
      updateSelectizeInput(session,
                           "uncertaintyvar",
                           selected = numcol()[2],
                           choices = numcol())
    })

    ## update input with the analyte list
    parlist <- reactive({
      req(input$parvar)

      datafile()[[input$parvar]] |> unique()
    })


    ## show the uploaded data into a table
    output$datatable <- DT::renderDT({
      req(isTRUE(dataok()))

      DT::datatable(
      datafile(),
      rownames = FALSE,
      style = "bootstrap4",
      fillContainer = TRUE,
      options = list(
        language = list(
          search = "Cerca",
          info = "Valori da _START_ a _END_ su _TOTAL_",
          lengthMenu = "Mostra _MENU_ valori",
          paginate = list(
            first = "Primo",
            last = "Ultimo",
            previous = "Precedente",
            `next` = "Successivo"
          )
          )
        )
      )
    })

    r$loadfile02 <- shiny::reactiveValues()

    observeEvent(input$nextbtn, {
      showModal(
        modalDialog(
          title = "Non potrai tornare indietro",
          "Hai assegnato correttamente tutte le variabili?",
          easyClose = TRUE,
          footer = tagList(
            actionButton(ns("yesbtn"), "S\u00EC"),
            modalButton("No")
          )
        )
      )
    })


    observeEvent(input$yesbtn, {
      req(isTRUE(dataok()))

      removeModal()

      r$loadfile02$data <- datafile()
      r$loadfile02$parvar <- input$parvar
      r$loadfile02$parlist <- parlist()
      r$loadfile02$responsevar <- input$responsevar
      r$loadfile02$secondresponsevar <- input$secondresponsevar
      r$loadfile02$uncertaintyvar <- input$uncertaintyvar

    })

  })
}
