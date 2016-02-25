# Number of elements in a lower triangle of a symmetric matrix of size n*n


#' Matrix: calculate a number of elements in a lower/upper triangle of a symmetric matrix of size n*n
#'
#' Number of elements in a lower/upper triangle of a symmetric matrix of size n*n
#'
#' @details
#' nCoeff = (n^2-n)/2, thus
#' n = (1+sqrt(1+8*nCoeff))/2
#'
#' @param n A number of rows/columns in a symmetric matrix.
#'
#' @return A number of elements in a lower/upper triangle of a matrix (excluding the diagonal).
#' @export
#' @seealso \link{nTri2nRow}, \link{simCorrVars}
#' @family Matrix operations
#' @examples
#'
#' nRow2nTri(5)
#' nRow2nTri(7)
#'
nRow2nTri <- function(n)  {nTri = (n ^ 2 - n)/2; return(nTri)}
# the same as #nTri = 0; for (i in (1:n)-1) nTri <- nTri + i;


# ================================================================================================

