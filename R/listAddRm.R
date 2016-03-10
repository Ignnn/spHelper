#' [!] Find and display added an removed names
#'
#' @param initial A charecter vector with initial names.
#' @param final A charecter vector with final names.
#'
#' @return A list with added and removed names
#' @export
#'
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna
#'
listAddRm <- function(initial, final) {
    Cols <- list()
    Cols$REMOVED <- initial[!(initial %in% final)]
    Cols$ADDED   <-   final[!(final   %in% initial)]

    # if (length(Cols$ADDED) > 0 ) {
    #     message("These columns were ADDED to the `hyperSpec` object:")
    #     cat(Cols$ADDED,sep = '\n')
    # }
    #
    # if (length(Cols$REMOVED) > 0 ) {
    #     message("These columns were REMOVED from the `hyperSpec` object:")
    #     cat(Cols$REMOVED,sep = '\n')
    # }
    return(Cols)
}
