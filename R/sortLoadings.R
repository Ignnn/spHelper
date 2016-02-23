# Sort component spectra =======================================================================
#
#' [!] Sort component spectra (a.k.a. loadings) by possition of top peak
#'
#' @description [!] = [INCOMPLETE DESCRIPTION] \cr
#'
#'  Sort component spectra (a.k.a. loadings) by
#'  possition of top peak and do additional tasks:
#'  \enumerate{
#'      \item{If \code{sp} is provided, convert resulting matrix to corresponding
#'          \code{\link[=hyperSpec-class]{hyperSpec}} object by using function
#'          \code{\link[hyperSpec]{decomposition}}.}

#'      \item{If \code{PCA = TRUE} and \code{sp} is provided, flips component's spectrum
#'          if mean of its scores is negative: (\code{sign(mean(Scores_of_component_i)) < 0})
#'          \code{loadings} and \code{sp} are used to calculate the scores.}
#'  }
#'
#' @template loadings
#' @template sp
#'
#' @param PCA Logical. If \code{TRUE}, some components are flipped. ... Set to TRUE
#' if PCA loadings are used. Default \code{PCA = FALSE}
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
#'
#'
#' @seealso More information at \code{\link[hyperSpec]{decomposition}}.
#'
#' @import hyperSpec
#' @export
#' @examples
#' ======================================================================
#'
#' sortLoadings(Loadings[c(2,3,5,1,4),,])
#'
#' ======================================================================
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
#' ======================================================================



sortLoadings <- function(loadings, sp = NULL, PCA = FALSE, sort = TRUE) {
    if (PCA & !is.null(sp)) { # flip
        ScoresTMP  <- getScores(hy2mat(sp), loadings)
        # ----------------------------------------------------------------------
        # Apverčiama, jei amplitudžių vidurkis neigiamas
        # signCoefs    <- sign(rowMeans(ScoresTMP))
        meanSign     <- function(x){sign(mean(x))}
        signCoefs    <- apply(ScoresTMP, MARGIN = 2, meanSign)
        loadings     <- sweep(loadings, MARGIN = 1, signCoefs,`*`)

        # # Normuojama
        # maxSpInt     <- apply(loadings, MARGIN = 1, max)
        # PCAvarimax2  <- sweep(loadings, MARGIN = 1, maxSpInt,`/`)

        # ======================================================================
    }

    # Rikiuojam iš eilės pagal matrcos eilučių maksimumo (y_max) vietą x ašy
    index.of.max <- apply(loadings, 1, which.max)
    OrderOfRows  <- order(index.of.max)

    # Viršūnių padėtis
    index.of.max <- index.of.max[OrderOfRows]

    if (sort == TRUE) {
        # Matrix with Sorted components
        loadings <- loadings[OrderOfRows,]
    } else {
        OrderOfRows <- 1:nrow(loadings)
    }

    if (!(is.null(sp))) {
        # Komponentus (išrikiuotas) paverčiam į "hyperSpec"" objektą
        loadings <- decomposition(sp, loadings,
                                  scores = FALSE,
                                  label.spc = "Comp. spektrum",
                                  retain.columns = F)
        # Suteikiam pavadinimus komponantams
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
