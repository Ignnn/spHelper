
#' Clear console (Matlab style)
#'
#' Clears console. The same as \code{CTRL + L}in RStudio.
#'
#' @export
#'
#' @family \pkg{spHelper} utilities


clc <- function() { cat("\014") }
