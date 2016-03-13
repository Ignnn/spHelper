
#' [!] Plot a color palette used in a hyperSpec object
#'
#' Plot color palette in a \code{\link[=hyperSpec-class]{hyperSpec}} object,
#'  added with function \code{link{hyAdd.color}}.
#'
#' @template  sp-hy
#' @param by \bold{Either} a name of factor variale in \code{sp} which levels
#'           correspond to colors in \code{palette} \bold{or}, if \code{sp} is
#'           not present, a charter vector of names to be ploted as a text.
#' @param palette A list of color names or color codes.
#' @param Title
#' @inheritParams hyAdd.color
#' @inheritParams base::legend
#'
#' @return A plot made with R \code{base} graphics system.
#' @export
#'
#' @examples
#'
#' sp <- hyAdd.color(Spectra3,"gr")
#' plot_hy.palette(sp, "gr")
#'
#' #--------------------------------------------------
#' plot_hy.palette(by = "RED color", palette = "#ee0000")
#'
#' @author Vilmantas Gegzna
#'
#' @family \pkg{spHelper} plots
#' @family \pkg{spHelper} functions for spectroscopy and \pkg{hyperSpec}
#'
plot_hy.palette <- function(sp = NULL, by = ".color",
                            palette = hyGet.palette(sp),
                            cex = 1.2,
                            Title = if (!is.null(sp)) "Colors of The Groups" else "Colors"){
    if (!is.null(sp)) {
        chk.hy(sp)
        TEXT <- levels(sp$..[[by]])

        if (length(TEXT) == 0) TEXT <- palette
    } else {
        TEXT <- if (by != ".color") as.character(by) else " "
    }

    if (length(palette) == 0)  {
        TEXT <- "— No colors are present —"
        palette <- NA
    }

    # If number of colors and strings do NOT match ----------------------------
    nText   <- length(TEXT)
    nColors <- length(palette)
    if ( nText != nColors){
        warning(sprintf(
            "Number of colors (n=%d) does not match number of text lines (n=%d).",
            nColors, nText))
        if (nText < nColors) {
            TEXT[(nText + 1):nColors] <- " "
        } else {
            palette[(nColors + 1):nText] <- NA
        }
    }

    # Plot --------------------------------------------------------------------
    plot.new()
    # title(main )
    legend("center", legend = TEXT, fill = palette, cex = cex,
           title = Title, bty = "n")
}


# TEXT <- factor(levels(Spectra$gr),levels(Spectra$gr))

# ggplot(mapping = aes(x = 1, y = 1:length(TEXT), fill = TEXT)) +
#
#     geom_tile()+
#     geom_text(label = TEXT) +
#     theme_void() +
#     scale_fill_manual(values = UsedColors)

