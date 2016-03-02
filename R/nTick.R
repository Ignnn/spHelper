#' @name nTick
#' @aliases nTick_x
#' @aliases nTick_y
#'
#' @title [+] Control number of ticks in \code{ggplot2} plots with continuous scale
#'
#' @description Convenience function to control number of ticks in \code{ggplot2}
#'  plots with continuous scale.
#'
#'
#' @param n An Integer giving the desired number of intervals. Non-integer
#'          values are rounded down.
#' @param min.n nonnegative integer giving the minimal number of intervals.
#'        If \code{min.n == 0}, \code{pretty(.)} may return a single value.
#' @param ... Other parameters to be passed to function \code{pretty}.
#'
#' @export
#' @seealso Pretty breakpoints: \code{\link[base]{pretty}}.
#' @examples
#'
#' # Make a plot but do not print
#' p <- plot_stat(Spectra, gr, mean, All.linetype = "solid") + facet_grid(.~gr)
#'
#' #Print the plot
#' p
#'
#' # Correct number of ticks:
#' p + nTick_x(2)
#' p + nTick_x(2) + nTick_y(8)
#'
#' @family spHelper plots


nTick_x <- function(n = 2, min.n = 2, ...){
    scale_x_continuous(breaks = number_ticks(n, min.n, ...))
}

#' @rdname nTick
#' @export
nTick_y <- function(n = 2, min.n = 2, ...){
    scale_y_continuous(breaks = number_ticks(n, min.n, ...))
}


#  ------------------------------------------------------------------------
#  [Internal function]
#  Function to to plot ticks
number_ticks <- function(n, min.n = 2, ...) {function(limits) pretty(limits, n, min.n = min.n,...)}
