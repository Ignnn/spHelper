
#' [!] Plot original, filtered signals and the noise
#'
#' Plot 2 signals and their difference (the noise).
#'
#' @param sp,sp_filt hyperSpec objects of the same size
#' @param ind An integer, that indicates which row must be plotted.
#' @param show.legend Logical
#' @param colors Vector of 3 colors for original, filtered and noise signals
#'              respectively.
#' @param legend.title The title of the legend.
#' @param legend.text Character vertor of length = 3. The entries for the legend
#'        for for original, filtered and noise signals respectively.
#'
#' @return A plot made with R \code{base} graphics system.
#' @inheritParams graphics::legend
#' @export
#'
#' @examples
#'
#' # Construct and apply running medians filter
#' medianFilt <- function(x) {runmed(x, 15)}
#'    sp_filt <- apply(Spectra, 1, medianFilt)
#'
#' plot_spFilt(Spectra, sp_filt, ind = 2)
#'
#' @family \pkg{spHelper} plots
#' @author Vilmantas Gegzna
plot_spFilt <- function(sp, sp_filt, ind = 1,
                        colors = c('green4','blue1', 'red'),
                        show.legend  = TRUE,
                        legend.title = "Signals",
                        legend.text  = c("Original","Filtered","Noise"),
                        lwd = 1){
    chk.hy(sp)
    chk.hy(sp_filt)
    if (length(ind) != 1)         stop("length(ind) != 1")
    if (length(colors) != 3)      stop("length(colors) != 3")
    if (length(legend.text) != 3) stop("length(legend.text) != 3")


    obj <- list()
    obj$Original <- sp[ind[1],"spc",]
    obj$Filtered <- sp_filt[ind[1],"spc",]
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

