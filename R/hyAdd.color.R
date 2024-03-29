#' [+] Add a variable with color names to \code{hyperSpec} object
#'
#' Add (or overwrite, if already exists) a column \code{.color} with color
#' names that correspond to levels of factor variable \code{by}.
#'
#' @template sp-hy
#' @param by A factor variale which levels will correspond to colors in
#'       \code{.color}.
#' @param palette A color palette (vector with colors for each level in
#'       \code{by}). If this argument is not provided, default palette is used.
#'
#' @return \code{HyperSpec} object with added/replaced column \code{.color}.
#'              Lables of variable \code{.color} indicate unique colors used
#'              (illustration in section "Examples").
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
#' # Overwrites labels/ color palette:
#' Spectra2$.color[1] <- "red"
#' hyGet.palette(Spectra2)
#' hyGet.palette(Spectra2)
#'     #>  Warning message:
#'     #>  In hyGet.palette(Spectra2) : Values of pallete do not exist.
#'     #>  Most probably they are overwriten by operation `$.color<-`.
#'
#' labels(Spectra2,".color")
#'     #>  ".color"
#'
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#' @author Vilmantas Gegzna

hyAdd.color <- function(sp = NULL, by,
                        palette = c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
                                    "#A65628","#F781BF","#999999")
                        ){
    # Define colors ------------------------------------------------------
    # c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
    #   "#A65628","#F781BF","#999999")
    # palette <- c("#377EB8","#4DAF4A","#984EA3","#FF7F00",
    #                 "#A65628","#F781BF","#999999")
    # RColorBrewer::brewer.pal(8,"Dark2")
    # # trellis.par.get("superpose.symbol")$col

    # ColorNumbers[is.na(ColorNumbers)] <- nlevels(sp$gr) + 1;

    by           <- getVarValues(by, sp)
    ColorNumbers <- unclass(as.factor(by));
    nColors <- max(ColorNumbers, na.rm = T)

    if (length(palette) < nColors) {
        stop(sprintf("At least %d colors must be in the palette.",nColors))
    }

    UniqueColors <- palette[1:nColors]

    # Add column for colors
    sp$.color <- NA
    sp$.color <- UniqueColors[ColorNumbers]

    # Labels is vector with color names
    labels(sp, ".color") <- UniqueColors
    return(sp)
}

