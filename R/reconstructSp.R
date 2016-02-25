# Reconstruct spectra --------------------------------------------------------
#
#' @name reconstructSp
#' @aliases reconstructSp
#' @aliases getReconstructed
#'
#' @title [!] Reconstruct spectra from loadings and scores.
#'
#' @description Reconstruct spectra from loadings and scores (i.e. )
#'
#' @template loadings
#' @template scores
#' @param sp original \code{hyperSpec} object. If \code{sp} is provided,
#' the result of this function will be the \code{sp} in which sp$spc
#' will be replaced with \code{reconstructed} spectra.
#'
#' @return \code{reconstructed <- scores \%*\% loadings}
#' @export
#'
#' @seealso  \code{\link[ChemometricsWithR]{reconstruct}} in \pkg{ChemometricsWithR}, \cr
#'          \code{\link[ChemometricsWithR]{project}} in \pkg{ChemometricsWithR},\cr
#'          \code{\link[Rssa]{reconstruct}} in \pkg{Rssa},\cr
#'          \code{\link[wmtsa]{reconstruct}} in \pkg{wmtsa}\cr
#'
#' @examples
#' function(Loadings, Scores)
#' function(Loadings, Scores, Spectra)
#'
#' @import hyperSpec
#'
reconstructSp  <-  function(loadings, scores, sp = NULL)     {
    reconstructed <- (hy2mat(scores)) %*% (hy2mat(loadings))
    if (class(sp) == "hyperSpec")   {
        sp$spc <- reconstructed; return(sp)
        } else  return(reconstructed)
}

#===========================================================================
#' @template same
#'
#' @rdname reconstructSp
#' @export
getReconstructed  <-  function(...,sp) {
    stop('Use function "reconstructSp" in stead of "getReconstructed".')
    }
