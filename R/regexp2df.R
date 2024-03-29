
# Regular expression (named tokens) ------------------------------------------------------------------

#' [+] Capture information to a dataframe by regular expressions
#'
#' Capture information in substrings of \code{text} that match named tokens
#' of regular expressions and convert the result to a data frame.
#'
#' @note Call to function \code{gregexpr} with parameter \code{perl = TRUE}
#' is used.
#'
#' @param text The text to be parsed: a character vector where matches are
#'        sought, or an object which can be coerced by \code{as.character}
#'        to a character vector.
#' @param pattern Perl-like regular expression.
#' @param ... Other arguments to be passed to \code{\link[base]{gregexpr}}.
#' @inheritParams base::gregexpr
#'
#' @return A data frame with parsed information.
#'
#' @details The syntax how to use named tokens is: \cr
#' '\emph{...}\bold{(?<}\emph{...}>\emph{...})\emph{...}' \cr
#' '\emph{expr1}\bold{(?<}\emph{Name}\bold{>}\emph{expr-to-capture}\bold{)}\emph{expr2}'
#'
#' \itemize{
#'   \item \bold{Name} - the name of the token.\emph{Note:} that spaces and other special symbols,
#'   inappropriate for variable names, are not allowed and will result in error.
#'   \item \bold{expr-to-capture} - regular expression to be captured as a
#'   value of a variable.
#'   \item \bold{expr1, expr2} - (optional) expressions, that must match, but
#'   that are not captured.
#' }
#'
#' @export
#'
#' @seealso @seealso More about regular expressions used in R: \link[base]{regex}\cr
#'
#'          \code{\link[base]{gregexpr}},
#'          \code{\link{regcapturedmatches}},
#'          \code{\link[dplyr]{\%>\%}}
#'
#' @examples
#' text1 <- c("A_111  B_aaa",
#'               "A_222  B_bbb",
#'               "A_333  B_ccc",
#'               "A_444  B_ddd",
#'               "A_555  B_eee")
#'
#' pattern1 <- 'A_(?<Part_A>.*)  B_(?<Part_B>.*)'
#'
#' regexp2df(text1, pattern1)
#'
#' ##     Part_A Part_B
#' ## 1    111    aaa
#' ## 2    222    bbb
#' ## 3    333    ccc
#' ## 4    444    ddd
#' ## 5    555    eee
#'
#' #----------------------------------------------------------
#' # Wrong. There must NOT be any SPACES in token's name:
#'
#' \donttest{
#' \dontrun{
#' pattern2 <- 'A (?<Part A>.*)  B (?<Part B>.*)'
#' regexp2df(text1, pattern2)
#'
#' ## Error ...
#'
#' }}
#' #----------------------------------------------------------
#' text3 <- c("sn555 ID_O20-5-684_N52_2_Subt2_01.",
#'               "sn555 ID_O20-5-984_S52_8_Subt10_11.")
#'
#' pattern3 <- paste0('sn(?<serial_number>.*) ID_(?<ID>.*)_(?<Class>[NS])',
#'                    '(?<Sector>.*)_(?<Point>.*)_[Ss]ubt.*\\.');
#'
#' regexp2df(text3, pattern3)
#'
#' ##   serial_number    ID       Class Sector Point
#' ## 1      555      O20-5-684     N     52     2
#' ## 2      555      O20-5-984     S     52     8
#'
#' #----------------------------------------------------------
#' # List all .R files in your working directory:
#'
#' regexp2df(dir(),'(?<R_file>.*\\.[rR]$)')
#'
#'
#' # Do the same by using chaining operator %>%:
#' library(dplyr)
#'
#' dir() %>% regexp2df('(?<R_file>\\.*[rR]$)')
#'
#' #----------------------------------------------------------
#' # Capture several types of files:
#'
#' expr <- paste0('(?<R_file>.*\\.[rR]$)|',
#'                '(?<Rmd_file>.*\\.[rR]md$)|',
#'                '(?<CSV_file>.*\\.[cC][sS][vV]$)')
#' dir() %>% regexp2df(expr)
#'
#' @family \pkg{spHelper} utilities
#' @author Vilmantas Gegzna

regexp2df <- function(text, pattern, ignore.case = FALSE, ...)  {
    ParsedData <- gregexpr(pattern, text, ignore.case, perl = TRUE, ...);
    as_a_list  <- regcapturedmatches(text, ParsedData)
    df <- do.call(rbind.data.frame, as_a_list)
    return(df)
}
