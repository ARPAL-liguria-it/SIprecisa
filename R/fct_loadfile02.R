#' Importing CSV files
#'
#' @description The function imports csv files trying to identify the
#' field and decimal separator. Intended csv files supported can have
#'  \itemize{
#'    \item{a comma as field separator and a dot as decimal separator;}
#'    \item{a semicolon as field separator and a comma as decimal separator;}
#'    \item{a semicolon as field separator and a dot as decimal separator.}
#'  }
#'
#' @param mypath a string with the path of the csv file to be imported.
#'
#' @return a {data.table} with the imported data. String are imported as factors.
#'
#' @export
#'
#' @import data.table
csvimport <- function(mypath){

  # reading the first row
  frow <- readLines(mypath, n= 1)

  # counting semicolons
  nsc <- lengths(regmatches(frow, gregexpr(";", frow)))

  # if no semicolons are detected, then the file is loaded with field delimiter
  # as comma and dot as decimal delimiter,
  # else the file is loaded with field separator
  # as semicolon and comma as decimal delimiter
  rawdata <- if(nsc == 0) {
    data.table::fread(mypath,
                      sep = ",", dec = ".",
                      header = "auto", stringsAsFactors = TRUE)
  } else {
    mydata <- data.table::fread(mypath,
                                sep = ";", dec = ",",
                                header = "auto", stringsAsFactors = TRUE)

    # some csv file may use semicolon as field delimiter and dot as decimal delimiter
    # to avoid numbers to be imported as character, the numeric columns are counted
    numcol <- sapply(mydata, is.numeric) |> sum()

    # then the columns with numbers separated by a dot are identified
    dotcol <- sapply(mydata, function(x) !any(grepl("[^0-9.]", x)))

    # only when numbers separated by dots are identified in character columns,
    # they are converted to numeric
    if(numcol == 0 && sum(dotcol) != 0) {
      mydata[, (names(dotcol)[dotcol]) := lapply(.SD,
                # character are imported as factors, and to convert to numeric
                # this little trick is required
                function (x) as.numeric(levels(x))[x]),
             .SDcols = dotcol]

    } else {

      mydata

    }

  }


}
