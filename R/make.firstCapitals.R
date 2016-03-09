#' @name make.firstCapitals
#' @aliases make.firstCapitals
#' @aliases fCap
#' @title [+] Convert first letters of all words to capitals
#'
#' @description  Convert first letters of all words in a string to capitals.
#' @details \code{fCap} is a wrapper of \code{make.firstCapitals}.
#'
#' @param x Either a string or a vector of strings.
#'
#' @return The same string as input with all words starting in capital letters.
#'
#' @examples
#'
#' make.firstCapitals('laa laa laa')
#' ##[1] "Laa Laa Laa"
#'
#' fCap('laa laa laa')
#' ##[1] "Laa Laa Laa"
#'
#' @export
#' @family \pkg{spHelper} utilities


make.firstCapitals <- function(x) {
    s <- strsplit(x, " ")[[1]]
    S <- paste(toupper(substring(s, 1, 1)),
            substring(s, 2),
            sep = "", collapse = " ")
    return(S)
}


#' @rdname make.firstCapitals
#' @export
fCap <- function(x){make.firstCapitals(x)}
