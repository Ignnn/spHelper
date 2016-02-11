# Convert hyperSpec object to matrix --------------------------------------
#
#' Extract matrix from either hyperSpec object or a matrix
#'
#' hy2mat
#'
#' @param Object - object of classes either \code{\link[=hyperSpec-class]{hyperSpec}}
#' or \code{\link[base]{matrix}}
#' @return A matrix or an error if it's impossible to extract a matrix.
#' @examples
#' hy2mat(Object)
#'
#' @export

hy2mat <- function(Object)
{    switch(class(Object),
            "hyperSpec" = Object$spc,
            "matrix"    = Object,
            stop('The class of the input must be either "hyperSpec" or "matrix"')
            )
}
