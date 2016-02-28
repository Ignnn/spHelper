#' [!] Add a variable with color names to hyperSpec object
#'
#' Add (or overwrite, if already exists) a column \code{.color} with color
#' names that correspond to values of factor variable \code{by}.
#'
#' @template sp
#' @param by Variale which levels will correspond to colors in \code{.color}.
#' @param palette A color palette (vector with colors for each level in \code{by}).
#'
#' @return HyperSpec object with added/replaced column \code{.color}. Lavel of
#'        \code{.color} indicate unique names of used colors.
#' @export
#'
#' @examples
#'
#' data(Spectra)
#' Spectra2 <- hyAdd.color(Spectra, "class")
#'
#' colnames(Spectra)
#' ## [1] "gr"    "class" "spc"
#'
#' colnames(Spectra2)
#' ## [1] "gr"     "class"  "spc"    ".color"
#'
#' @import hyperSpec
#' @family \code{spHelper} functions for \code{hyperSpec}

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

    ColorNumbers <- unclass(as.factor(sp[[, by]][,1]));
    nColors <- max(ColorNumbers, na.rm = T)

    if (length(palette) < nColors) stop(sprintf("At least %d colors must be in the palette.",nColors))

    UniqueColors <- palette[1:nColors]

    # Add column for colors
    sp$.color <- NA
    sp$.color <- UniqueColors[ColorNumbers]

    # Labels is vector with color names
    labels(sp, ".color") <- UniqueColors
    return(sp)
}
