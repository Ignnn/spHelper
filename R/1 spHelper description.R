
#' Various functions by V. Gegzna, et al.
#'
#' Functions in spHelper by topic
#'
#' @section Cross-validation:
#'
#' \code{\link{createFolds_strat}} \cr
#' \code{\link{createFolds_stratified}} \cr
#'
#' @section Component analysis / Spectroscopy:
#'
#' \code{\link{sortLoadings}} \cr
#' \code{\link{getScores}} \cr
#' \code{\link{InfoDim}} \cr
#' \code{\link{InfoDim_plot}} \cr
#' \code{\link{GaussAmp}} \cr
#'
#'
#' @section Spectroscopy / \code{\link{hyperSpec}}:
#'
#' \code{\link{hy2mat}} \cr
#' \code{\link{read3csv2hy}} \cr
#' \code{\link{addLabels_TD2009}} \cr
#'
#'
#' @section Plotting:
#'
#' \code{\link{plot_kAmp}} \cr
#' \code{\link{plot_kSp}} \cr
#' \code{\link{plot_kSpFacets}} \cr
#' \code{\link{PlotConfusion}} \cr
#'
#'
#' @section Regular expressions:
#'
#' \code{\link{regcapturedmatches}} \cr
#' \code{\link{regexpr2df}} \cr
#'
#'
#' @section Various:
#'
#' \code{\link{makeFirstCapital}} \cr
#'
#' @examples
#' # List all functions in package:
#'
#' FunctionList <- unclass(lsf.str(envir = asNamespace("spHelper"), all = T))
#' pander::pander(as.data.frame(FunctionList))
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