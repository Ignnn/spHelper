# Reconstruct spectra --------------------------------------------------------
#
#' @name reconstructSp
#' @aliases reconstructSp
#' @aliases getReconstructed
#'
#' @title [!] Reconstruct spectra from loadings and scores.
#'
#' Reconstruct spectra from loadings and scores (i.e. )
#'
#' @param loadings ??? loadings
#' @param scores ??? scores
#' @param sp original \code{hyperSpec} object. If \code{sp} is provided,
#' the result of this function will be the \code{sp} in which sp$spc
#' will be replaced with \code{reconstructed} spectra.
#'
#' @return \code{reconstructed <- scores \%*\% loadings}
#' @export
#'
#' @seealso \code\{\link[ChemometricsWithR]{reconstruct}} \pkg{ChemometricsWithR} \cr
#'          \code\{\link[ChemometricsWithR]{project}} \pkg{ChemometricsWithR} \cr
#'
#'          \code\{\link[Rssa]{reconstruct}} \pkg{Rssa} \cr
#'          \code\{\link[wmtsa]{reconstruct}} \pkg{wmtsa} \cr
#'
#' @examples
#' function(loadings, scores)
#' function(loadings, scores, sp)
#'
reconstructSp  <-  function(loadings, scores, sp = NULL)     {
    reconstructed <- (hy2mat(scores)) %*% (hy2mat(loadings))
    if (class(sp)=="hyperSpec")   {
        sp$spc <- reconstructed; return(sp)
        } else  return(reconstructed)
}

#' @export
#' @rdname reconstructSp
getReconstructed  <-  function(...) getReconstructed(...)
