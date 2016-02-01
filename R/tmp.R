


#' [!] Reconstruct spectra from loadings and scores.
#'
#' Reconstruct spectra from loadings and scores (i.e. )
#'
#' @param loadings ??? loadings
#' @param scores ??? scores
#' @param Sp original \code{hyperSpec} object. If \code{Sp} is provided,
#' the result of this function will be the \code{Sp} in which Sp$spc
#' will be replaced with \code{reconstructed} spectra.
#'
#' @return \code{reconstructed <- scores \%*\% loadings}
#' @export
#'
#' @examples
#' function(loadings, scores)
#' function(loadings, scores, Sp)
#'
getReconstructed  <-  function(loadings, scores, Sp = NULL)
    {
    reconstructed <- (hy2mat(scores)) %*% (hy2mat(loadings))
    if (class(Sp)=="hyperSpec")   {
        Sp$spc <- reconstructed; return(Sp)} else  return(reconstructed)
    }

#  ------------------------------------------------------------------------

