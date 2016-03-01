#' [~] Parse arguments of a function and return approriate values for selected variable
#'
#' Parse dataframe \code{DATA}, variable \code{VAR} and a call
#' \code{CALL} of a function and return approriate values of the
#' \code{VAR}. \cr If possible, return \code{DATA[,"VAR"]} \cr
#' Otherwise return \code{VAR}.
#'
#' @param VAR A name of a variable (with or without quotes).
#' @param DATA A name of a data frame (with or without quotes).
#' @param CALL (\code{\link[=call-class]{Call}}) to be parsed.
#'
#' @return A vector. If possible, return \code{DATA[,"VAR"]}.
#' Otherwise return \code{VAR}.
#' @export
#'
#' @examples
# EXAMPLE 1 *****************************************************************
#' # Data
#' df       <- mtcars[,c("cyl","gear")]
#' variable <- c("my","vector")
#' #  ------------------------------------------------------------------------
#' f1 <- function(data, v1, v2){
#'     CALL <- match.call()
#'     out  <- getVarValues(v1, data, CALL) # <<< function `getVarValues`
#'     return(out)
#' }
#'
#' # Returns values of variable in data frame `df`:
#' f1(df, cyl)
#' f1(df, gear)
#'
#' # Returns values of "free" variable
#' # (i.e., values of variable in caller function's environment):
#' f1(df, variable)
#'
# EXAMPLE 2 *****************************************************************
#' # A Data frame
#'    df <- data.frame(A = "Values_A_(DATA.FRAME)",
#'                     E = "Values_E_(DATA.FRAME)")
#'
#' # Vectors
#'    A <- "Values of the vector 'A'"
#'    B <- "Values of the vector 'B'"
#'
#'
#' # A call object `CALL`:
#'
#' fun  <- function(data, gr, ID) match.call()
#' CALL <- fun(df, A, B)
#' CALL
#' ## fun(data = df, gr = A, ID = B)
#'
#' # Possible outputs -----------------------------------------------------------------------
#'
#' getVarValues(VAR = gr, DATA = df, CALL = CALL)
#' ## [1] Values A (DATA.FRAME)
#'
#' getVarValues(gr, df, CALL)
#' ## [1] Values A (DATA.FRAME)
#'
#' getVarValues(A, df, CALL)
#' ## [1] "Values of the vector 'A'"
#'
#' getVarValues(B, df, CALL)
#' ## [1] "Values of the vector 'B'"
#'
#'
#' # UNEXPECTED results -----------------------------------------------------------------------
#'
#' \donttest{
#' \dontrun{
#'
#'  getVarValues(ID, df, CALL) # ??? `ID` found only in function's `fun` definition.
#'  ## NULL
#'
#'  getVarValues(G, df, CALL) # ERROR, as variable G does not exist.
#'  ##  Error in eval(expr, envir, enclos) : object 'G' not found
#'
#'  getVarValues(F, df, CALL) # F is a special variable: F = FALSE
#'  ##  FALSE
#'
#'  getVarValues(c, df, CALL) # c() is a function.
#'  ## function (..., recursive = FALSE)  .Primitive("c")
#' }}
getVarValues <- function(VAR, DATA, CALL){
    # getVarValues(VAR, DATA, CALL = match.call())

    # Look for missing arguments-------------------------------------
    missVar <- vector("logical",3)
    missVar[1] <- missing(VAR)
    missVar[2] <- missing(DATA)
    missVar[3] <- missing(CALL)
    if (any(missVar)) {
        missVarTXT <- paste(c("VAR", "DATA", "CALL")[missVar],
                            collapse = ", ")
        stop(paste("Missing arguments with no default:", missVarTXT))
    }
    # ---------------------------------------------------------------
    # Get parent environment
    env <- parent.frame()

    # Convert input values to character
    VAR   <- as.character(match.call()$VAR)
    DATA  <- as.character(match.call()$DATA)


    # Get a value of variable in the call object `CALL`. This value is a
    # name of variable of inerest:
    Name <- CALL[[VAR]] # name is a `call` object

    if (!is.null(Name)) {
        Name <- as.character(c(Name)); # c() is used to convert `call` object to
 		                               # a string correctly.
        txt2 <- paste0(VAR," <- if('",Name,"' %in% colnames(env$",DATA,")) ",
                       "env$", DATA, "$", Name,
                       " else env$", VAR)
        eval(parse(text  =  txt2))
    }
    return(eval(parse(text  =  VAR)))
}
