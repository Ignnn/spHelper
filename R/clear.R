
#' [!] Clear variables from workspace (global environment)
#'
#' Remove items from the workspace, i.e. the global environment,
#' and free up system memory. Explanation in section \bold{"Details"}.
#'
#' @details
#' \code{clear} removes either listed or all variables (if none is listed) from
#'  workspace (global environment). Default is to clear all objects.\cr\cr
#' \code{clear.except} clears all variables except listed ones.\cr\cr
#' \code{clear.class} removes objects of indicated class(es), except those which
#' names provided as argument \code{except}.\cr\cr
#' \code{clear.except.class} keeps objects of indicated class(es), others are
#'  cleared.\cr\cr
#' \code{clear.fun} removes \emph{all} functions only. \cr\cr
#'
#' @param ... The objects in the global environment as names (unquoted) or
#'            character strings (quoted).
#'
#' @param list A character vector naming objects used instead of `\code{...}`.
#'        If \code{list} is not \code{NULL}, `\code{...}` is ignored.
#'
#' @param except Names of \bold{variables} (as a character vector) in the global
#'        environment \bold{to be kept} (to be cleared). Default is \code{NULL}.
#' @param clrClass Names of classes (as a character vector). Objects of indicated classes
#'        will be removed from workspace (global environment).
#' @param exceptClass Names of classes (as a character vector) in the global environment
#'        that has \bold{not} to be cleared. Default is \code{NULL}.
#'
#' @export
#'
#' @examples
#'
#' clear()
#'
#'   A <- 5
#'   B <- "s"
#'  D1 <- "string2"
#'  D2 <- "string3"
#'   L <- list(A,B)
#' FUN <- function(x) x
#' ls()
#'
#' clear.class("numeric")
#' ls()
#'
#' clear.fun()
#' ls()
#'
#' clear(except = c("D1", "D2"))
#' ls()
#'
#' clear(except = c("B"))
#' ls()
#'
#' clear()
#' ls()
#'
#' A1 <- 5
#' A2 <- 5
#' A3 <- 5
#' B <- list("a","A")
#' c <- "ABC"
#' ls()
#'
#' clear.except.class(c("numeric", "list"))
#' ls()
#'
#' clear.class("numeric", except = "A1")
#' ls()
#'
#' --------------------------------------------
#' # BUG: clear.fun() gives error if special function (e.g. `%>%`) is present.
#'
#' @family \pkg{spHelper} utilities
#' @family \pkg{spHelper} \code{clear} family functions
#'
#' @seealso \code{\link[base]{rm}}
#' @source Ideas to create \code{clear} is taken from \code{\link[pracma]{clear}}
#'         in \pkg{pracma}.
#' @author Vilmantas Gegzna
#
# @param pos The environment as a position in the search list.

clear <- function(... , list = NULL, except = NULL) {
    if (is.null(list)) {
        list <- match.call(expand.dots = FALSE)$`...`
        list <- unlist(lapply(list, as.character))
    }

    if (is.null(list)) {list <- ls(name = .GlobalEnv)}
    list <- setdiff(list, except)

    if (!is.character(list))
        stop("Argument must be empty or a character vector.")

    rm(list = list, envir = .GlobalEnv)
    gc()
    invisible("Cleared")


    # @param clr Names of variables (as a character vector)
    #        that has \bold{to be cleared}. Default is to clear all.
}
#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
#'
clear.except <- function(..., list = NULL)  {
    if (is.null(list)) {
        list <- match.call(expand.dots = FALSE)$`...`
        list <- unlist(lapply(list, as.character))
    }
    if (!is.null(list)) {
        clear(except = list)
    } else {
        warning("The workspace is not cleared as no variables that must be kept are listed.")
    }
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
            if (any(item %in% except)) next
            if (any(itemClass %in% clrClass)) clear(list = item)
        }

        invisible("Cleared")
    }
}
#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
clear.except.class <- function(exceptClass = NULL) {
    if (!is.null(list)) {
        clrList <- ls(globalenv())
        if (length(clrList) == 0) {
            return(invisible(NULL))
        } else {
            for (item in clrList) {
                itemObj   <- eval(parse(text = item), globalenv())
                itemClass <- class(itemObj)
                if (any(itemClass %in% exceptClass)) next else clear(list = item)
            }

            invisible("Cleared")
        }
    } else {
        warning("The workspace is not cleared as no classes that must be kept are listed.")
    }
}
#  ------------------------------------------------------------------------
#' @rdname clear
#' @export
clear.fun <- function() { clear.class("function") }




