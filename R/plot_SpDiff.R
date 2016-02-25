# ***** ***** -----------------------------------------------------
#
#' [!] Plot Remainders After Subtracting Components
#'
#' Plot difference between experimental and reconstructed spectra \cr \cr
#'
#' Plot difference between experimental (original) and reconstructed spectra.
#' Uses function \code{\link{getReconstructed}}, to calculate the reconstructed
#' spectra and subtracts it from original spectra. \cr
#'
#' Difference between experimental and reconstructed spectra'
#'
#'
#' @param loadings loadings
#' @param scores scores
#' @param Spectra Spectra (hyperSpec object)
#' @param Title Title of the plot.
#' @param spc.nmax max number of spectra to plot
#' @param color ...
#' @param stacked ...
#'
#' @return Plot of calculated difference between expected (original)
#' and reconstructed spectra.
#' @export
#'
#' @examples
#'
#' plot_SpDiff(Loadings, Scores, Spectra)
#'
plot_SpDiff <- function(loadings,scores,Spectra,
                        Title = 'Remainders After Subtracting Components',
                        color = if (".color" %in% ls(Spectra$..)) Spectra$.color else "tan3",
                        stacked = if ("ID" %in% ls(Spectra$..)) Spectra$ID else NULL,
                        spc.nmax = 2000) {
    SpRE <- reconstructSp(loadings,scores,Spectra)

    plot(Spectra - SpRE,
         spc.nmax = spc.nmax,
         col = color,
         stacked = stacked,
         title.args = list(main = Title)
    )
}
