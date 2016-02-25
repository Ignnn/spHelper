# Remove empty columns ----------------------------------------------------

#' [!] Remove hyperSpec object columns that contain only \code{NA} values
#'
#' @template sp
#'
#' @return Object without columns that contain all NA values.
#' @export
#'
#' @import hyperSpec
#' @family hyperSpcec supplements
#'
dropCol <- function(sp){
    NAcols <- colSums(is.na(sp$..)) == nrow(sp)
    dropNames  <- names(NAcols)[NAcols]
    sp <- sp[,!(colnames(sp) %in% dropNames)]
    return(sp)
}
