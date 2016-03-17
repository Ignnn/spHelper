
#' [!] Compare 2 spectroscopic signals
#'
#' Compare 2 spectroscopic signals: plot 2 signals and their difference.
#'
#' @param sp1,sp2 hyperSpec objects of the same size
#' @param row An integer, that indicates which row must be plotted.
#' @param show.legend Logical
#' @param colors Vector of 3 colors for original, filtered and noise signals
#'              respectively.
#' @param legend.title The title of the legend.
#' @param legend.text Character vertor of length = 3. The entries for the legend
#'        for for original, filtered and noise signals respectively.
#' @param lwd A line width. More details in \link[graphics]{par}.
#'
#' @return A plot made with R \code{base} graphics system.
#' @inheritParams graphics::legend
#' @export
#'
#' @examples
#'
#' # Construct and apply running medians filter
#' Original <- Spectra
#' Filtered <- apply(Original, 1, function(x) {runmed(x, 15)})
#'
#' plot_spCompare(Original, Filtered, row = 2)
#'
#' @family \pkg{spHelper} plots
#' @author Vilmantas Gegzna
plot_spCompare <- function(sp1, sp2, row = 1,
                        colors = c('green4','blue3', 'red'),
                        show.legend  = TRUE,
                        legend.title = "Signals",
                        legend.text  = c("Original","Filtered","Noise"),
                        lwd = 1){
    chk.hy(sp1)
    chk.hy(sp2)
    if (length(row) != 1)         stop("length(row) != 1")
    if (length(colors) != 3)      stop("length(colors) != 3")
    if (length(legend.text) != 3) stop("length(legend.text) != 3")


    obj <- list()
    obj$Original <- sp1[row[1],"spc",]
    obj$Filtered <- sp2[row[1],"spc",]
    obj$Noise    <- obj$Original  - obj$Filtered


    obj <- hyperSpec::collapse(obj)
    # obj$.type   <- factor(c(2,2,1), c(1,2), c(legend.title, legend.text[3]))
    obj$.type   <- factor(c(2,2,1), c(1,2), c(' ', '  '))

    obj$.colors <- colors
    obj$lwd     <- lwd

    obj <- obj[c(3,1,2),,]
    plotspc(obj,
         stacked = obj$.type,
         col     = obj$.colors,
         lines.args = list(lwd = obj$lwd))

    if (show.legend == TRUE) {
        legend("topright", title = legend.title,
               legend = legend.text,
               col = colors,
               lty = 1,
               lwd = lwd)
    }
}

