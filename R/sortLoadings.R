# Sort component spectra =======================================================================
#
#' [+] Sort component spectra (a.k.a. loadings) by of top peak
#'
#' @description Sort rows of \code{hyperSpoec} object by possition of maximum
#'  value in rows (in other words, sort component spectra, a.k.a. loadings, by
#'  possition of top peak) and do additional tasks:
#'  \enumerate{
#'      \item{If \code{sp} is provided, convert resulting matrix to corresponding
#'          \code{\link[=hyperSpec-class]{hyperSpec}} object by using function
#'          \code{\link[hyperSpec]{decomposition}}.}
#'
#'      \item{If \code{PCA = TRUE}, \code{sp} is provided, and if the mean of
#'      i-th component's scores is negative, flip the loadings ot that component:
#'       (\code{sign(mean(Scores_of_component_i)) < 0})
#'          \code{loadings} and \code{sp} are used to calculate the scores.}
#'  }
#'
#' @template loadings
#' @template sp
#'
#' @param PCA Logical. If \code{TRUE}, some components are flipped. Set to TRUE
#' if PCA loadings are used. Default \code{PCA = FALSE}. The flipping follows
#' the rule:
#' \deqn{loading_i * (-score_i) = (-loading_i) * score_i}{loading[i] * (-score[i]) = (-loading[i]) * score[i]}
#' where \eqn{-loading-i}{-loading[i]} represents the i-th flipped loading.
#'
#' @param sort Logical. Indicates if returned componenst must be sorted.
#'       If \code{FALSE}, only additional tasks are performed.
#'       Default is \code{TRUE}.
#'
#' @return Either matrix (if \code{sp} is not provided) or
#' \code{hyperSpec} object with sorted loadings.
#' In case of \code{hyperSpec} object, 3 additional columns
#' (PeakAt, order.of.rows, kNames) are added.
#'
#' @note spectra (object of class \code{\link[=hyperSpec-class]{hyperSpec}})
#'          which will be used to convert sorted loadings into
#'          \code{\link[=hyperSpec-class]{hyperSpec}} object.
#'
#'
#' @export
#' @examples
#' # ======================================================================
#'
#' sortLoadings(Loadings[c(2,3,5,1,4),,])
#'
#' # ======================================================================
#'
#' loadings <- Loadings[[]] # Exrtract matrix of spectra
#'
#' L1 <- sortLoadings(loadings)         # returns a matrix
#' class(L1)
#' ## [1] "matrix"
#'
#' L2 <- sortLoadings(loadings,Spectra) # returns a hyperSpec object
#' class(L2)
#' ## [1] "hyperSpec"
#' # ======================================================================
#' @seealso \code{\link[hyperSpec]{decomposition}}
#' @family component analysis / factorisation related functions in \pkg{spHelper}
#' @author Vilmantas Gegzna

sortLoadings <- function(loadings, sp = NULL, PCA = FALSE, sort = TRUE) {
    if (PCA & !is.null(sp)) { # flip
        ScoresTMP  <- getScores(hy2mat(sp), loadings)
        # ----------------------------------------------------------------------
        # Flip, if average of amplitudes/scores is negative
        # signCoefs    <- sign(rowMeans(ScoresTMP))
        meanSign     <- function(x){sign(mean(x))}
        signCoefs    <- apply(ScoresTMP, MARGIN = 2, meanSign)
        loadings     <- sweep(loadings,  MARGIN = 1, signCoefs,`*`)

        # # Normalize
        # maxSpInt     <- apply(loadings, MARGIN = 1, max)
        # PCAvarimax2  <- sweep(loadings, MARGIN = 1, maxSpInt,`/`)

        # ======================================================================
    }

	# Sort in accordance with the position of matrix's row maxima
	#  (y_max) on x axis
    index.of.max <- apply(loadings, 1, which.max)
    OrderOfRows  <- order(index.of.max)

    # Position of maxima
    index.of.max <- index.of.max[OrderOfRows]

    if (sort == TRUE) {
        # Matrix with Sorted components
        loadings <- loadings[OrderOfRows,]
    } else {
        OrderOfRows <- 1:nrow(loadings)
    }

    if (!(is.null(sp))) {
        # Convert (sorted) components to "hyperSpec"" object
        loadings <- decomposition(sp, loadings,
                                  scores = FALSE,
                                  label.spc = "Comp. spektrum",
                                  retain.columns = F)
        # Create names of components
        PeakAt    <- make.unique(paste0(round(wl(loadings)[index.of.max]),
                                        "nm"),"_")
        # WARNING is needed, if variables with names are already present
        loadings$PeakAt        <- PeakAt
        loadings$kNames        <- paste0("max: ", PeakAt)
        loadings$order.of.rows <- OrderOfRows

        labels(loadings,'spc') <- labels(sp,'spc')
    }
    return(loadings)
}
