#'  [!!!] Construct a correlation matrix
#'
#' @param vec A Vector with known correlation coefficients to construct
#'            a correlation matrix.
#'
#' @return A constructed correlation matrix.
#' @export
#'
#' @family Matrix operations
#' @family simmulation functions
#'
#' @examples
#'
#' # ------------------------------------------------------------
#' # Example 1A: Vector into a matrix
#'
#' vector = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
#' corrVec2Mat(vector)
#'
#' # ------------------------------------------------------------
#'
#' \donttest{
#' \dontrun{
#'
#'  # Example 1B: Vector is transformed to a matrix by filling
#'  # it column-wise:
#'
#'   ## vector = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)
#'   ##
#'   ##  matrix =
#'   ##       [,1] [,2] [,3] [,4]
#'   ##  [1,]   .     .   .    .
#'   ##  [2,]  0.1    .   .    .
#'   ##  [3,]  0.2  0.4   .    .
#'   ##  [4,]  0.3  0.5  0.6   .
#'   ##
#'   ## In this example only the matrix elements of interest are shown.
#'
#'  # ------------------------------------------------------------
#'  # Example 2: inappropriate number of coefficients - warning appears
#'
#'  corrVec2Mat(vector[1:5])
#'   ## Warning message:
#'   ## Only first 3 coefficient(s) out of 5 will be used to construct symmetric matrix with 3 rows.
#'
#'  # ------------------------------------------------------------
#'  # Example 3: ERROR appears - coeefs must be between [-1;1]
#'
#'  corrVec2Mat(1:5)
#'   ## Error in corrVec2Mat(1:5) :
#'   ## All values in input vector must be between [-1 and 1]
#' }}
#'
corrVec2Mat <- function(vec){

    if (!all(abs(vec) <= 1)) stop("All values in input vector must be between [-1 and 1]")

    nCoeff  <- length(vec)
    n       <- nTri2nRow(nCoeff) # calculate number of summetric matrix rows
    n.floor <- floor(n)
    n.ceil  <- ceiling(n)

    if (n.floor != n.ceil) { # if not an integer
        warning(sprintf('Only first %d coefficient(s) out of %d will be used to construct symmetric matrix with %d rows.',
                        nRow2nTri(n.floor), nCoeff,n.floor))
        n   <- n.floor
        vec <- vec[1:n] # prevent additional warning with `lower.tri()`
    }

    # Generate an idendity matrix (ones in diagonal)
    M <- diag(n)

    # Fill lower part of the matrix with coefficients
    M[lower.tri(M)] <- vec

    # Make the matrix symmetric
    M[upper.tri(M)] <- t(M)[upper.tri(M)]

    return(M)
}
