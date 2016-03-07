
#' Clear console (Matlab style)
#'
#' Clears console. The same as \code{CTRL + L}in RStudio.
#'
#' @export
#'
#' @family family \pkg{spHelper} utilities


clc <- function() { cat("\014") }
