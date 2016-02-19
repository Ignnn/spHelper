#' Matrix: calculate a dimension of a symmetric matrix by a number elements in a lower/upper triangle
#'
#' @details Dedived from equation nCoeff = (n^2-n)/2
#' @param nTri A number of elements in a lower/upper triangle of a matrix (excluding the diagonal).
#'
#' @return A number of rows/columns in a symmetric matrix. May be a fractional number.
#' @export
#' @seealso \link{nRow2nTri}, \link{simCorrVars}
#' @examples
#' nTri2nRow(10)
#' nTri2nRow(6)
#'
nTri2nRow <- function(nTri){n <- (1+sqrt(1+8*nTri))/2; return(n)}
