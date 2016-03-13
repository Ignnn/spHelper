
#' Insert \%>\%.
#'
#' Call this function as an addin to insert \code{ \%>\% } at the cursor position.
#'
#' @export
#' @family \pkg{spHelper} addins
insertPipeline_Addin <- function() {
    rstudioapi::insertText(text = " %>% ")
}

#' Insert \%*\%.
#'
#' Call this function as an addin to insert \code{ \%*\% } at the cursor position.
#'
#' @export
#' @family \pkg{spHelper} addins
insertMatMuliplication_Addin <- function() {
    rstudioapi::insertText(text = " %*% ")
}


#' Insert \%in\%.
#'
#' Call this function as an addin to insert \code{ \%in\% } at the cursor position.
#'
#' @export
#' @family \pkg{spHelper} addins
insertIn_Addin <- function() {
    rstudioapi::insertText(text = " %in% ")
}
