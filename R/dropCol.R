# Remove empty columns ----------------------------------------------------

#' [!] Remove colums of hyperSpec object with all NA values
#'
#' @template sp
#'
#' @return Object without columns that contain all NA values.
#' @export
#'
#' @examples
#'
#'  dropCol(sp)
#'
dropCol <- function(sp){
    NAcols <- colSums(is.na(sp$..)) == nrow(sp)
    dropNames  <- names(NAcols)[NAcols]
    sp <- sp[,!(colnames(sp) %in% dropNames)]
    return(sp)
}
