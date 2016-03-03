#' @name makeFirstCapital
#' @aliases makeFirstCapital
#' @aliases fCap
#' @title [+] Convert first letters of all words to capitals
#'
#' @description  Convert first letters of all words in a string to capitals.
#' @details \code{fCap} is a wrapper of \code{makeFirstCapital}.
#'
#' @param x Either a string or a vector of strings.
#'
#' @return The same string as input with all words starting in capital letters.
#'
#' @examples
#'
#' makeFirstCapital('laa laa laa')
#' ##[1] "Laa Laa Laa"
#'
#' fCap('laa laa laa')
#' ##[1] "Laa Laa Laa"
#'
#' @export
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna

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
