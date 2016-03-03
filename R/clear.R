
#' [.] Clear variables from workspace (global environment)
#'
#' Clear variables from workspace (global environment).
#'
#' @param lst Character vector of names of variables in the global environment.
#'
#' \code{clear} removes either all ol listed  variables from workspace
#'               (global environment).\cr
#' \code{clear.class} removes objects of indicated class(es).\cr
#' \code{clear.fun} removes functions only.
#'
#' @param className Name of class. Objects of this class will be removed
#' from workspace (global environment).
#' Remove all items from the workspace, i.e. the global environment,
#' and freeing up system memory.
#'
#' @export
#'
#' @source Function \code{clear} is copied from \code{\link[pracma]{clear}}
#'         in \pkg{pracma}.
#' @examples
#'
#' A <- 5
#' B <- "s"
#' L <- list(A,B)
#' FUN <- function(x) x
#'
#' clear.class("numeric")
#' clear.fun()
#' clear()
#'
#' @family `spHelper` utilities
#'
clear <- function(lst) {
    if (missing(lst))
        lst <- ls(name = .GlobalEnv)

    if (!is.character(lst))
        stop("Argument must be empty or a character vector.")

    rm(list = lst, envir = globalenv())
    null <- gc()
}

#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
clear.class <- function(className) {

    lslist <- ls(globalenv())

    if (isempty(lslist)) return(invisible(NULL))

    for (item in lslist) {
        itemObj   <- eval(parse(text = item), parent.frame())
        itemClass <- class(itemObj)
        if (any(itemClass %in% className)) clear(item)
    }

    invisible("Cleared")
}

#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
clear.fun <- function() { clear.class("function") }
