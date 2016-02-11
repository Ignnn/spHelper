# Remove empty columns ----------------------------------------------------

#' [!] Remove colums of hyperSpec object with all NA values
#'
#' @param Spectra - hyperSpec object
#'
#' @return
#' @export
#'
#' @examples
#'
#'  dropCol(Spectra)
#'
dropCol <- function(Spectra){
    NAcols <- colSums(is.na(Spectra$..)) == nrow(Spectra)
    dropNames  <- names(NAcols)[NAcols]
    Spectra <- Spectra[,!(colnames(Spectra) %in% dropNames)]
    return(Spectra)
}
