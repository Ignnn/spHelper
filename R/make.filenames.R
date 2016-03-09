
#' [+] Convert string into Windows compatible filename
#'
#' Function replaces special symbols \emph{\ / : * ? " < > |} in a string \code{s}
#'  with underscore (\code{_}). After replacement string can be used as a file
#'  name in Windows.
#'
#'
#' @param s A string.
#' @param allow.space Logical. If \code{FALSE}, space symbols will be repalaced
#'       to underscores (\code{_}).
#'
#' @return A string without special symbols.
#' @export
#'
#' @examples
#'  s <- '\\ / : * ? " < > |'
#'  make.filenames(s)
#'  #> "_ _ _ _ _ _ _ _ _"
#'
#'  make.filenames(s,allow.space = FALSE)
#'  #> "_________________"
#'
#'
#'  s2 <- "Hello?"
#'  make.filenames(s2)
#'  #> "Hello_"
#'
#'
#' @family family \pkg{spHelper} utilities
make.filenames <- function(s, allow.space = TRUE){
    s <- gsub('[\\\\/\\:\\*\\?\\"\\<\\>\\|]','_',s)
    if (allow.space == FALSE)
        s <- gsub('[[:space:]]','_',s)
    return(s)
}
