# Padaryti pirmą raidę didžiąja -------------------------------------------
#
#
#' @name makeFirstCapital
#' @aliases makeFirstCapital
#' @aliases fCap
#' @title [!] Convert the first letter to capital.
#'
#' @description   [!] Convert the first letter to capital.
#' @details \code{fCap} is a wrapper of \code{makeFirstCapital}
#'
#' @param x - a string or vector of strings
#'
#' @return The same string with all words starting with capital letter.
#'
#' @examples
#'
#' makeFirstCapital('laa laa laa')
#'
#' ##[1] "Laa Laa Laa"
#'
#' @export

makeFirstCapital <- function(x) {
    s <- strsplit(x, " ")[[1]]
    S <- paste(toupper(substring(s, 1, 1)),
            substring(s, 2),
            sep = "", collapse = " ")
    return(S)
}


#' @rdname makeFirstCapital
#' @export
fCap <- function(x){makeFirstCapital(x)}
