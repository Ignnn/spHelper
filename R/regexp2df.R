
# Regular expression (named tokens) ------------------------------------------------------------------
#' [!] Use regular expressions to capture substrings to dataframe
#'
#' Capture substrings that match regular expression's named tokens
#' and convert the result to a data frame.
#'
#'
#' @param strings - strings to be parsed.
#' @param pattern - Perl-like regular expression.
#'
#' @return A data frame.
#'
#' @details The syntax how to use named tokens is: \cr
#' '\emph{...}\bold{(?<}\emph{...}>\emph{...})\emph{...}' \cr
#' '\emph{expr1}\bold{(?<}\emph{Name}\bold{>}\emph{expr-to-capture}\bold{)}\emph{expr2}'
#'
#' \itemize{
#'   \item \bold{Name} - the name of token. Any spaces or other special symbols,
#'   inappropriate for variable names, are not allowed and will result in error.
#'   \item \bold{expr-to-capture} - regular expression to be captured as a
#'   value of a variable.
#'   \item \bold{expr1, expr2} - (optional) expressions, that must match, but
#'   that are not captured.
#' }
#'
#' @export
#'
#' @seealso \code{\link[base]{gregexpr}},
#'          \code{\link{regcapturedmatches}},
#'          \code{\link[dplyr]{\%>\%}}
#'
#' @examples
#' strings1 <- c("A_111  B_aaa",
#'               "A_222  B_bbb",
#'               "A_333  B_ccc",
#'               "A_444  B_ddd",
#'               "A_555  B_eee")
#'
#' pattern1 <- 'A_(?<Part_A>.*)  B_(?<Part_B>.*)'
#'
#' regexp2df(strings1, pattern1)
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
#' regexp2df(strings1, pattern2)
#'
#' ## Error ...
#'
#' }}
#' #----------------------------------------------------------
#' strings3 <- c("sn555 ID_O20-5-684_N52_2_Subt2_01.",
#'               "sn555 ID_O20-5-984_S52_8_Subt10_11.")
#'
#' pattern3 <- paste0('sn(?<serial_number>.*) ID_(?<ID>.*)_(?<Class>[NS])',
#'                    '(?<Sector>.*)_(?<Point>.*)_[Ss]ubt.*\\.');
#'
#' regexp2df(strings3, pattern3)
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


regexp2df <- function(strings, pattern)  {
    ParsedData <- gregexpr(pattern,strings, perl = TRUE);
    as_a_list  <- regcapturedmatches(strings,ParsedData)
    df <- do.call(rbind.data.frame, as_a_list)
    return(df)
}
