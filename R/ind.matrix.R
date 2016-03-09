#' Create a matrix filled with its element indices
#'
#' Genereta a matrix of the same size as provided matrix \code{x}, where each
#' element represents its index.
#'
#' @param x A matrix.
#'
#' @export
#' @examples
#'
#'  ind.matrix(matrix(NA, 2, 5))
#'  ind.matrix(matrix(NA, 5, 2))
#'
#' @family matrix operations
#' @seealso \link[base]{row},\link[base]{col}
#' @author Vilmantas Gegzna
#'
ind.matrix <- function(x){
    x      <- as.matrix(x)
    dims   <- dim(x)
    m      <- matrix(1:length(x),nrow = dims[1],ncol = dims[2])
    return(m)
}
