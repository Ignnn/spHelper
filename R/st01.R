#' [+] Standardize values to intervat from min to 1
#'
#' Apply formula: \eqn{(x-m)/(1-m)}{(x-m)/(1-m)}
#' @param x A numeric vector vector
#' @param m A value treated as a minimum.
#'
#' @export
#'
#' @examples
#'
#' x <- seq(.3,1,.1)
#' st01(x)
#' st01(x, m = .2)
#'
#' @family family \pkg{spHelper} utilities
st01 <- function(x, m = min(x)){
    (x-m)/(1-m)
}

