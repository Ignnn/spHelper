
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
#'
#' data(Spectra)
#' Spectra2 <- hyAdd.color(Spectra, "class")
#'
#' colnames(Spectra)
#'    #> [1] "gr"    "class" "spc"
#'
#' colnames(Spectra2)
#'    #> [1] "gr"     "class"  "spc"    ".color"
#'
#' # Names of colors, used for each level of factor variable
#' labels(Spectra2,".color")
#'    #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' hyGet.palette(Spectra2)
#'    #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' # ATTENTION -------------------------------------------------
#'
#' # Preserve labels/ color palette:
#' Spectra2[1,".color"] <- "red"
#' hyGet.palette(Spectra2)
#'     #>  "#377EB8" "#4DAF4A" "#984EA3" "#FF7F00"
#'
#' # Overwrite labels/ color palette:
#' Spectra2$.color[1] <- "red"
#' hyGet.palette(Spectra2)
#'     #>  Warning message:
#'     #>  In hyGet.palette(Spectra2) : Values of pallete do not exist.
#'     #>  Most probably they are overwriten by operation `$.color<-`.
#'
#' labels(Spectra2,".color")
#'     #>  ".color"
#'
#' @family functions for \pkg{hyperSpec}
#' @author Vilmantas Gegzna
hyGet.palette <- function(sp){
    if (class(sp) == "hyperSpec") {
        Palette <- labels(sp,".color")
        if (is.null(Palette)) Palette <- unique(sp$.color)
        if (any(Palette == ".color")) {
            warning(paste0("Values of pallete do not exist.\n",
                          "Most probably they are overwriten by operation `$.color<-`."))
            invisible(NULL)
        } else {
        if (length(Palette) == 1) warning("Only 1 unique color was found!")
        return(Palette)
        }
    } else {
        return(NULL)
    }
}
