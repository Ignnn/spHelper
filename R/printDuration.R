

#' Print duration (difference of time)
#'
#' @param StartAt Time, when period of time started
#'       (usually generated with \code{Sys.time()})
#' @param Message Message before time stamp. Default is
#'         \code{"Duration of analysis:"}
#'
#' @return Text with time duration printed in console.
#' @export
#'
#' @examples
#' StartAt <-  Sys.time()
#'
#' printDuration(StartAt)
#' printDuration(StartAt,"From start till now")
#'
printDuration <- function(StartAt, Message = "Duration of analysis:"){
    Duration_of_analysis <- Sys.time() - StartAt;
    AnDuration <- paste(Message,
                        round(Duration_of_analysis, 1),
                        attributes(Duration_of_analysis)$units
    )
    cat(AnDuration, sep = "\n")
}
