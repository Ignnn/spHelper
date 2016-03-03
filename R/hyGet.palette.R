
#' [+] Get color palette used to create variable'.color'
#'
#' @param sp A \code{hyperSpec} object, that contais variable \code{.color}
#'  added with function \code{\link{hyAdd.color}}
#'
#' @return Color palette used to create variable \code{.color}.
#' @note If class of \code{sp} is not \code{hyperSpec}, function returns
#'       \code{NULL}.
#' @export
#'
#' @examples
#' data(Spectra)
#' Spectra2 <- hyAdd.color(Spectra, "class")
#'
#' # Names of colors, used for each level of factor variable
#'
#' hyGet.palette(Spectra2)
#'    #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' labels(Spectra2,".color")
#'    #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' @family functions for \pkg{hyperSpec}
#' @author Vilmantas Gegzna
hyGet.palette <- function(sp){
    if (class(sp) == "hyperSpec") {
        Palette <- labels(sp,".color")
        if (is.null(Palette)) Palette <- unique(sp$.color)
        if (length(Palette) == 1) warning("Only 1 unique color was found!")
        return(Palette)
    } else {
        return(NULL)
    }
}
