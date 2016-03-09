
#' [!] Clear variables from workspace (global environment)
#'
#' Remove items from the workspace, i.e. the global environment,
#' and free up system memory.
#'
#'  \cr
#' \code{clear} removes either listed or all variables from workspace
#'               (global environment).\cr
#' \code{clear.except} clears all variables except listed ones.
#' \code{clear.class} removes objects of indicated class(es).\cr
#' \code{clear.fun} removes functions only. \cr
#'
#'
#' @param clr Names of variables (character vector) in the global environment
#'        that has \bold{to be cleared}. Default - clear all.
#' @param except Names of variables (character vector) in the global environment
#'        that has \bold{not} to be cleared . Default is \code{NULL}.
#'
#' @param clrClass Names of classes (character vector). Objects of indicated classes
#'        will be removed from workspace (global environment).
#'
#' @export
#'
#' @examples
#'
#' A <- 5
#' B <- "s"
#' D1 <- "string2"
#' D2 <- "string3"
#' L <- list(A,B)
#' FUN <- function(x) x
#'
#' clear.class("numeric")
#' clear.fun()
#' clear(except = c("D1", "D2"))
#' clear(except = c("B"))
#' clear()
#'
#' @family \pkg{spHelper} utilities
#' @source Ideas to create \code{clear} is taken from \code{\link[pracma]{clear}}
#'         in \pkg{pracma}.
#' @author Vilmantas Gegzna
clear <- function(clr = ls(name = .GlobalEnv), except = NULL) {

    clr <- setdiff(clr, except)

    if (!is.character(clr))
        stop("Argument must be empty or a character vector.")

    rm(list = clr, envir = globalenv())
    null <- gc()
}

#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
clear.class <- function(clrClass = NULL, except = NULL) {
    clrList <- ls(globalenv())

    if (length(clrList) == 0) {
        return(invisible(NULL))
    } else {

        for (item in clrList) {
            itemObj   <- eval(parse(text = item), globalenv())
            itemClass <- class(itemObj)
            if (any(itemClass %in% except)) next
            if (any(itemClass %in% clrClass)) clear(item)
        }

        invisible("Cleared")
    }
}

#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
clear.fun <- function() { clear.class("function") }

#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
#' @param ... The objects to be removed, as names (unquoted) or character strings (quoted).
#' @param list A character vector naming objects to be removed. If \code{list}
#' is not \code{NULL}, \code{...} is ignored.
#'
clear.except <- function(..., list = NULL)  {
    if (is.null(list)) {
        list <- match.call(expand.dots = FALSE)$`...`
        list <- unlist(lapply(list, as.character))
    }
    if (!is.null(list)) {
        clear(except = list)
    } else {
        warning("The workspace is not cleared as no function arguments were provided.")
    }
}


