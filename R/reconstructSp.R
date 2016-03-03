# Reconstruct spectra --------------------------------------------------------
#
#' @name reconstructSp
#' @aliases reconstructSp
#' @aliases getReconstructed
#'
#' @title [+] Reconstruct spectra from loadings and scores (i.e. components and amplitudes)
#'
#' @description Reconstruct spectra from loadings and scores (i.e. from
#' spectroscopic components and their amplitudes).
#'
#' @template loadings
#' @template scores
#' @param sp \code{hyperSpec} object which has the same number or rows as
#'  \code{scores}. If \code{sp} is provided, reconstructed spectra will replace
#' other spectroscopic data in \code{sp}. This object will be returned as a
#' result of this function.
#'
#' @return Reconstruction of the original spectroscopic data matrix, based on
#'         \code{scores} and \code{loadings} as either a matrix or
#'         \code{hyperSpec} object. \cr
#'      \code{reconstructed <- scores \%*\% loadings}
#' @export
#'
#' @seealso  \code{\link[ChemometricsWithR]{reconstruct}} in \pkg{ChemometricsWithR}, \cr
#'          \code{\link[ChemometricsWithR]{project}} in \pkg{ChemometricsWithR},\cr
#'          \code{\link[Rssa]{reconstruct}} in \pkg{Rssa}
#'
#' @examples
#' function(Loadings, Scores)
#' function(Loadings, Scores, Spectra)
#'
#' @import hyperSpec
#' @family component analysis / factorisation related functions
#' @author Vilmantas Gegzna
#'
reconstructSp  <-  function(loadings, scores, sp = NULL)     {
    reconstructed <- (hy2mat(scores)) %*% (hy2mat(loadings))
    if (class(sp) == "hyperSpec")   {
        sp$spc <- reconstructed
        return(sp)            # hyperSpec object
    } else  {
        return(reconstructed) # matrix
    }
}

#===========================================================================
#' @template same
#' @rdname reconstructSp
#' @export
getReconstructed  <-  function(...,sp) {
    stop('Use function "reconstructSp" in stead of "getReconstructed".')
    }
