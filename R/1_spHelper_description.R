
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
#' \code{\link{createFolds_stratified}} \cr
#'
#' @section Component analysis / Spectroscopy:
#'
#' \code{\link{sortLoadings}} \cr
#' \code{\link{GaussAmp}} \cr
#' \code{\link{getScores}} \cr
#' \code{\link{InfoDim}} \cr
#' \code{\link{InfoDim_plot}} \cr
#' \code{\link{whichOutlier}} \cr
#'
#'
#' @section Spectroscopy / \code{\link{hyperSpec}}:
#'
#' \code{\link{hy2mat}} \cr
#' \code{\link{read3csv2hy}} \cr
#'
#' \code{\link{addLabels_TD2009}} \cr
#'
#'
#' @section Plotting:
#'
#' \code{\link{plot_kAmp}} \cr
#' \code{\link{plot_kSp}} \cr
#' \code{\link{plot_kSpFacets}} \cr
#' \code{\link{Plot_SpDiff}}    \cr
#' \code{\link{PlotConfusion}}  \cr
#' \code{\link{withSubTitle}}  \cr
#' \code{\link{subt}}          \cr
#' \code{\link{InfoDim_plot}} \cr
#' \code{\link{getReconstructed}} \cr
#'
#'
#' \code{\link{}} \cr
#' \code{\link{}} \cr
#' \code{\link{}} \cr
#' \code{\link{}} \cr
#' \code{\link{}} \cr
#' \code{\link{}} \cr
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
#' \code{\link{makeFirstCapital}} \cr
#' \code{\link{list.functions}} \cr
#' \code{\link{dropCol}} \cr
#' \code{\link{}} \cr
#' \code{\link{}} \cr
#' \code{\link{}} \cr
#'
#'
#'
#' @examples
#' # List all functions in package:
#'
#' listFunctions()
#'
#'
#' Plot structure of functions inside the package:
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
#'
#'
#'
#'
#'
#'
#'
#' # Other things to remember
#' devtools::install('D:/Data/R/spHelper')
#' library(spHelper)
#'
#'
#' plotc(ObjectName[,,500],model = spc~Integration_time)
#'
#' @docType package
#' @name spHelper
NULL
#> NULL
