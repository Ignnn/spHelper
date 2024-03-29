
#' Various functions by V. Gegzna, et al.
#'
#'
#'
#' @description
#' [+] - function is well described. \cr
#'
#' [!] - a description is incomplete and needs revision.\cr\cr
#'
#' Functions in spHelper by topic
#'
#' @section Cross-validation:
#'
#' \code{\link{stratifiedFolds}} \cr
#'
#' @section Component analysis / Spectroscopy:
#'
#' \code{\link{sortLoadings}} \cr
#' \code{\link{GaussAmp}} \cr
#' \code{\link{getScores}} \cr
#' \code{\link{infoDim}} \cr
#' \code{\link{qplot_infoDim}} \cr
#' \code{\link{whichOutlier}} \cr
#'
#'
#' @section Spectroscopy / \code{\link[=hyperSpec-class]{hyperSpec}}:
#'
#' \code{\link{hy2mat}} \cr
#' \code{\link{read3csv2hy}} \cr
#'
#' \code{\link{hyAdd.Labels_TD2009}} \cr
#'
#'
#' @section Plotting:
#'
#' \code{\link{qplot_kAmp}} \cr
#' \code{\link{qplot_kSp}} \cr
#' \code{\link{qplot_kSpFacets}} \cr
#' \code{\link{qplot_confusion}}  \cr
#' \code{\link{qplolt_spStat}}  \cr
#' \code{\link{plot_spDiff}}    \cr
#'
#'
#'
#' \code{\link{subt}}          \cr
#' \code{\link{qplot_infoDim}} \cr
#' \code{\link{reconstructSp}} \cr
#'
#'
#' @section Regular expressions:
#'
#' \code{\link{regcapturedmatches}} \cr
#' \code{\link{regexp2df}} \cr
#'
#'
#' @section Various:
#'
#'
#' \code{\link{bru}} \cr
#' \code{\link{fCap}} \cr
#' \code{\link{make.firstCapitals}} \cr
#' \code{\link{list.functions}} \cr
#' \code{\link{hyDrop.NA}} \cr
#'
#'
#' @author Vilmantas Gegzna
#'
#' @import ggplot2
#' @importFrom tidyr '%>%'
#' @examples
#'
#' \donttest{
#' \dontrun{
#'
#' # List all functions in package:
#'
#' list.functions()
#'
#' # Plot structure of functions inside the package:
#' require(sna)
#' require(mvbutils)
#'
#' pkgFW <- mvbutils::foodweb(where="package:spHelper", cex=0.7, charlim=60)
#' sna::gplot(pkgFW$funmat, g = 9,
#'            jitter = T,
#'            # mode = "mds",
#'            label.cex = .6,
#'            diag=TRUE,
#'            vertex.cex=1:2,
#'            displaylabels=TRUE,
#'            label.bg="gray90")
#'
#'
#' # Other things to remember
#'
#' devtools::install('D:/Data/R/spHelper')
#' library(spHelper)
#'
#'  devtools::build_vignettes()
#'
#' plotc(ObjectName[,,500],model = spc~Integration_time)
#' }}
#' @docType package
#' @name spHelper
#'
NULL
#> NULL

# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
# \code{\link{}} \cr
