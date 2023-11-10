#' Number of decimals for given significant figures
#'
#' @description The function returns the number of decimals required for given
#'  significant figures.
#'
#' @param value a number for which the number of decimals required to get the
#'    desired number of significant figures should be calculated
#' @param signif the number of desired significant figures
#'
#' @return The number of decimals required for the \code{signif} significant figures.
#'
#' @noRd
signiftodigits <- function(value,
                           signif = 4L) {

  stopifnot(is.numeric(value),
            all.equal(signif, as.integer(signif)))

  if (is.infinite(value)){
    0
  } else {

  # the number is converted to text with the desired significant figures
  sprintf_txt <- paste0("%#.", signif, "g")
  value_text <- sprintf(sprintf_txt, value)

  # splitting integers from decimals
  value_digits <- strsplit(value_text, "\\.")
  value_digits[[1]][[1]] <- gsub("-", "" , value_digits[[1]][[1]])

  # counting the integers
  integers <- value_digits[[1]][[1]]
  value_integers <- nchar(integers)

  # if the integers are not enough for the requested significant figures,
  # or the data has been expressed in scientific notation, the number of decimals
  # is 0, otherwise, the decimals are counted.
  if(value_integers < signif) {
  # counting the decimals
  decimals <- ifelse(length(value_digits[[1]]) == 1,
                     paste0(rep("0", signif - value_integers), collapse = ""),
                     value_digits[[1]][[2]])
  value_decimals <- nchar(decimals)

  # checking if the number is scientific notation
  is_scientific <- "e" %in% strsplit(decimals, "")[[1]]

  ndigits <- ifelse(is_scientific, 0, value_decimals)
  ndigits

  } else {

    ndigits <- 0
    ndigits
    }
  }
}

#' Formatting a number with a given number of significant figures
#'
#' @description The function returns a character values with a number formatted
#' with the desired significant figures
#'
#' @param number the input number to be formatted
#' @param sigfig an integer with the number of desired significant figures
#'
#' @return a character value with the number formatted with the desired
#' significant figures
#'
#' @noRd
format_sigfig <- function(number, sigfig = 4L){
  stopifnot(
    is.numeric(number),
    is.integer(sigfig)
  )
sprintf("%.*f", signiftodigits(number, sigfig), number)

}

#' Conversion of an HTML formatted string to a RMarkdown string
#'
#' @description The function substitutes some common HTML tag to their
#'  RMarkdown counterparts. Handled HTML tags are \code{<h4></h4>},
#'  \code{<h5></h5>}, \code{<ul></ul>}, \code{<li></li>}, \code{<b></b>},
#'  \code{<i></i>} and \code{</br>}.
#'
#' @param htmlstring the HTML string to be converted.
#'
#' @return a string with the HTML tags replaced by their RMarkdown counterparts.
#'
#' @noRd
htmltormarkdown <- function(htmlstring){

  stopifnot(
    !is.null(htmlstring)
  )

  if (is.na(htmlstring)) {

    NA

  } else {

  htmlstring |>
    (\(x) gsub("<h4>", "\n###", x) )() |>
    (\(x) gsub(" </h4>", "  \n" , x) )() |>
    (\(x) gsub("<h5>", "\n####", x) )() |>
    (\(x) gsub("</h5>", "  \n", x) )() |>
    (\(x) gsub("<ul>", "\n", x) )() |>
    (\(x) gsub("</ul>", "  \n", x) )() |>
    (\(x) gsub("<li>", "\n  *", x) )() |>
    (\(x) gsub("</li>", "", x) )() |>
    (\(x) gsub("\u03b1", "$\\\\alpha$", x) )() |>
    (\(x) gsub("\u03bd", "$\\\\nu$", x) )() |>
    (\(x) gsub("\u2013", "\\\\textendash\\\\", x) )() |>
    (\(x) gsub("\u21e8", "\n $\\\\Rightarrow$", x) )() |>
    (\(x) gsub("\u00b1", "$\\\\pm$", x) )() |>
    (\(x) gsub("R\u00B2", "$\\\\mathrm{R}^2$", x) )() |>
    (\(x) gsub("\u2264", "$\\\\leq$", x) )() |>
    (\(x) gsub("\u2260", "$\\\\neq$", x) )() |>
    (\(x) gsub("<b>", "**", x) )() |>
    (\(x) gsub("</b>", "**", x) )() |>
    (\(x) gsub("<i>", "_", x) )() |>
    (\(x) gsub("</i>", "_", x) )() |>
    (\(x) gsub("</br>", "  \n ", x) )() |>
    (\(x) gsub("E<sub>n</sub>", "$E_n$", x) )() |>
    (\(x) gsub("\u03C7<sup>2</sup>", "$\\\\chi^2$", x) )() |>
    (\(x) gsub("\u03C7\u00B2", "$\\\\chi^2$", x) )()
  }
}

#' Rendering of an RMarkdown report as future promise
#'
#' @description The function passes some parameters to a RMarkdown file for
#'  automatic reporting. The report is processed as a \code{future_promise}
#'  from the {promises} package.
#'
#' @param input the {rmd} report template.
#' @param output a temporary file for writing the content of the new report.
#' @param params the parameters to be passed to the remport template.
#'
#' @return the function passes the parameters to a {Rmd} report template as
#'  {future_promise}
#'
#' @noRd
#' @importFrom rmarkdown render
#' @importFrom promises future_promise
render_report <- function(input, output, params) {
  promises::future_promise({
  rmarkdown::render(input,
                    output_file = output,
                    params = params,
                    envir = new.env(parent = globalenv())
                    )
  }, seed = TRUE)
}

#' {\%notin\%} operator
#'
#' @description The \code{\%notin\%} operator returns the opposite result of the
#'  \code{\%in\%} operator.
#'
#' @noRd
`%notin%` = Negate(`%in%`)

#' Bslib card with help tips
#'
#' @description The function provides a {bslib} card with help text.
#' The help text is stored in Rmarkwdown file placed into the \code{/inst/rmd}
#' package folder.
#'
#' @param card_title the title of the card. Default is "Help".
#' @param rmdfile the name of the Rmarkdown file with the help instructions.
#' The file must be placed in the \code{/inst/rmd} folder of the package.
#' @param rmdpackage the package name.
#'
#' @return a {bslib} card with the help text.
#'
#' @noRd
#' @importFrom bslib card card_header card_body
#' @importFrom shiny icon
help_card <- function(card_title,
                      rmdfile,
                      rmdpackage) {

  rmdpath <- parse(text = sprintf("system.file('rmd', '%s', package = '%s')",
                                  rmdfile, rmdpackage)) |>
    eval()

  bslib::card(
    bslib::card_header(icon("hammer"), card_title),
    bslib::card_body(
      includeMarkdown(rmdpath)
      )
    )

}

#' Bslib accordion with help tips
#'
#' @description The function provides a {bslib} accordion with help text
#' arranged on three panels.
#' The help text is stored in Rmarkwdown files placed into the \code{/inst/rmd}
#' package folder.
#'
#' @param todotitle the title for the first panel.
#' @param tipstitle the title for the second panel.
#' @param togettitle the title for the third panel.
#' @param todofile the name of the Rmarkdown file with the help instructions.
#' @param tipsfile the name of the Rmarkdown file with the help instructions.
#' @param togetfile the name of the Rmarkdown file with the help instructions.
#'
#' @details Files must be placed in the \code{/inst/rmd} folder of the package.
#'
#' @return a {bslib} accordion with three panels.
#'
#' @noRd
#' @importFrom bslib accordion accordion_panel
#' @importFrom shiny icon includeMarkdown
help_accordion <- function(todotitle,
                           tipstitle,
                           togettitle,
                           todofile,
                           tipsfile,
                           togetfile) {

  todopath <- parse(text = sprintf("system.file('rmd', '%s', package = '%s')",
                                   todofile, package = "SIprecisa")) |>
    eval()

  tipspath <- parse(text = sprintf("system.file('rmd', '%s', package = '%s')",
                                   tipsfile, package = "SIprecisa")) |>
    eval()

  togetpath <- parse(text = sprintf("system.file('rmd', '%s', package = '%s')",
                                    togetfile, package = "SIprecisa")) |>
    eval()

  bslib::accordion(
    id = "help",
    open = "todo",
    bslib::accordion_panel(icon = shiny::icon("hammer"),
                    title = todotitle,
                    value = "todo",
                    shiny::includeMarkdown(todopath)),
    bslib::accordion_panel(icon = shiny::icon("lightbulb"),
                    title = tipstitle,
                    value = "tips",
                    shiny::includeMarkdown(tipspath)),
    bslib::accordion_panel(icon = shiny::icon("vials"),
                    title = togettitle,
                    tips = "toget",
                    shiny::includeMarkdown(togetpath))
  )

}

#' Horizontal line for {plotly} plots
#'
#' @description The function makes a list suitable for drawing an horizontal line
#' in a {plotly} plot
#'
#' @param y a numeric value with the coordinate of the line to be plotted.
#' @param color a character value with the color of the line.
#' @param dash a character value for the line appearance. Possible values are
#' "solid", "dot", "dash", "longdash", "dashdot", or "longdashdot". Default is "solid".
#'
#' @details Files must be placed in the \code{/inst/rmd} folder of the package.
#' Saved from \url{https://stackoverflow.com/questions/34093169/horizontal-vertical-line-in-plotly}
#' Carson's answer.
#'
#' @return a {list} to be used inside the {plotly} {layout} function.
#'
#' @examples
#' plotly::plot_ly() |>
#'   plotly::layout(shapes = list(hline(5)))
#'
#' @noRd
#' @importFrom plotly plot_ly layout

hline <- function(y = 0,
                  color = "blue",
                  dash = "solid") {

  stopifnot(
    is.numeric(y),
    is.character(color),
    is.character(dash),
    dash %in% c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot")
  )

  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,
                dash = dash)
  )
}
