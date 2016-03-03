#' [.] Convert expressions in \code{ggplot2} object labels to strings
#'
#' Convert expressions in \code{\link[ggplot2]{ggplot}} object labels to strings, to
#' enable them to be plotted with \code{\link[plotly]{ggplotly}}.
#' @param p ggplot2 object
#'
#' @return `ggplot2` object
#' @export
#'
#' @examples
#' data(flu, package = "hyperSpec")
#'
#' qplotspc(flu)
#' ggplotly()
#'
#' rmExpr()
#' ggplotly()
#'
#' #--------------------------------
#' p1 <- plot_sp(Loadings)
#' p1$labels
#'
#' p2 <- rmExpr(p1)
#' p2$labels
#'
#' ggplotly(p1)
#' ggplotly(p2)
#'
#' #----------------------------------
#' \donttest{
#' \dontrun{
#' qplotspc(aggregate(chondro, chondro$clusters, mean_pm_sd),
#' mapping = aes(x = .wavelength,
#'               y = spc,
#'               colour = clusters)) +
#'     facet_grid(clusters ~ .) + ggtitle("Spectra of Chondrocytes")
#'
#'  rmExpr()
#'  ggplotly()
#'
#' }}
#'
#' @family spHelper plots
#' @family `spHelper` utilities
#' @author Vilmantas Gegzna
rmExpr <- function(p = ggplot2::last_plot()) {
    p$labels  <- lapply(p$labels, expr2text)
    return(p)
}

#' [.] Unwrap text from `call` object
#' Assumption: only the second element in a `call` can be text
#' first element is an object of class "name", that can be dropped.
#' This algorithm can loose some text elements.
#'
#' @param x A `call` object
#'
#' @return A string.
#' @export
#' @family `spHelper` utilities
#' @author Vilmantas Gegzna
uncall <- function(x){

    x <- as.list(x)[-1]
    unwrapCall <- function(y) {
        while (is.call(y)) {y <- as.list(y)[[2]]};
        y
    }
    x <- lapply(x, unwrapCall)
    paste(unlist(x),collapse = ", ")
}


#' [.] Convert `expression` and `call` to text and remove quotes
#'
#' @param x `Call` object or `expression`
#'
#' @export
#' @family `spHelper` utilities
#' @author Vilmantas Gegzna
#'
expr2text <- function(x) {
    x <- switch(class(x),
                # expression = simsalapar::escapeLatex(as.call(x)),
                expression = {tryCatch(to_math_q(x),
                                       error = function(e) as.character(x))},
                call = uncall(x),
                paste(as.character(x),collapse = " "))

    x <- gsub('(^\")|(\"$)',"", x)
    x <- gsub('\"',", ", x)
    return(x)
}
