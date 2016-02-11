#' Print a string of repeated symbols. Useful for highlighting results.
#'
#' String of repeated symbols. Useful for r Markdown (.rmd) files and console output
#' to highlight results.
#'
#' @param symbol - desired symbol or sequence of symbols. Default is "="
#' @param len    - length of a line \cr default is 60.
#' @param after - number of new (empty) lines/rows to be added afterwards.
#'        \code{0} means that following text continues in the same row.
#'        Default is \cr\code{if (print==TRUE) 1 else 0}.
#' @param before - number of peceeding emptyrows. Default is 0.
#' @param print - if \code{TRUE} (defailt) - print,
#'                if \code{FALSE} - return as a string
#'
#' @return String of repeated symbols
#' @export
#'
#' @examples
#'
#' bru
#' bru("-")
#' bru("= ")
#'
bru <- function(symbol = "=",
                len = 60,
                after  = {if (print) 1 else 0},
                before = 0,
                print  = TRUE)
{
    # Create sequences of symbols
    nlA <- paste0(rep('\n', after), collapse = "")
    nlB <- paste0(rep('\n', before),collapse = "")
    lineC <- paste0(rep(symbol,len),  collapse = "")
    # Adjust the length
    lineC <- substr(lineC,1,len)
    # Join all symbols
    lineC <- paste0(nlB, lineC, nlA)
    # Either print or return the result
    if (print)  cat(lineC) else return(lineC)
}
