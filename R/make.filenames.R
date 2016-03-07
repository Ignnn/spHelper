
#' [+] Convert string into Windows compatible filename
#'
#' Function replaces symbols \ / : * ? " < > | in a string \code{s}
#'  with underscore. After replacement string can be used as a file name
#' in Windows.
#'
#'
#' @param s A string.
#'
#' @return A string without spacial symbols.
#' @export
#'
#' @examples
#'  s <- '\\ / : * ? " < > |'
#'  make.nameswin(s)
#'  #> "_ _ _ _ _ _ _ _ _"
#'
#'  s2 <- "Hello?"
#'  make.nameswin(s2)
#'  #> "Hello_"
#'
#'
#' @family family \pkg{spHelper} utilities
make.nameswin <- function(s){
    gsub('[\\\\/\\:\\*\\?\\"\\<\\>\\|]','_',s)
}
