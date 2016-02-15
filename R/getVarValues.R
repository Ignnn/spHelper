#' [!] Parse a call of a function and return approriate values for a variable.
#'
#' Parse dataframe \code{data}, variable \code{variable} and a call
#' \code{CALL} of a function and return approriate values of the
#' \code{variable}. \cr If possible, return \code{data[,variable]} \cr
#' Otherwise return \code{variable}.
#'
#' @param variable A name of a variable (with or without quotes).
#' @param data A name of a data frame (with or without quotes).
#' @param CALL (\code{\link[=call-class]{Call}}) to be parsed.
#' @param env Envoronmen to search for "free" variables in.
#'
#' @return
#'#' @export
#'
#' @examples
#'
#' # A Data frame
#'    df <- data.frame(A = "Values A (DATA.FRAME)",
#'                     E = "Values E (DATA.FRAME)")
#'
#' # Vectors
#'    A <- "Values of the vector 'A'"
#'    B <- "Values of the vector 'B'"
#'
#'
#' # A Call object `CALL`:
#'
#' fun  <- function(data, gr, ID) match.call()
#' CALL <- fun(df, A, B)
#' CALL
#' ## fun(data = df, gr = A, ID = B)
#'
#' # Possible outputs -----------------------------------------------------------------------
#'
#' getVarValues(variable = gr, data = df, CALL = CALL)
#' ## [1] Values A (DATA.FRAME)
#'
#' getVarValues(A, df, CALL)
#' ## "Values of the vector 'A'"
#'
#' getVarValues(B, df, CALL)
#' ##
#'
#' getVarValues(gr, df, CALL)
#' ## Values A (DATA.FRAME)
#'
#' # UNEXPECTED results -----------------------------------------------------------------------
#'
#' getVarValues(ID, df, CALL) # `ID` is neither in `df`, nor in environment `env`
#' ## NULL
#'
#' getVarValues(G, df, CALL) # ERROR, as variable G does not exist.
#' ##  Error in eval(expr, envir, enclos) : object 'G' not found
#'
#' getVarValues(F, df, CALL) # F is a special variable: F = FALSE
#' ##  FALSE
#'
#' getVarValues(c, df, CALL) # c() is a function.
#' ## function (..., recursive = FALSE)  .Primitive("c")
#'
getVarValues <- function(variable, data, CALL, env = parent.frame()){
    # Look for missing arguments-------------------------------------
    missVar <- vector("logical",3)
    missVar[1] <- missing(variable)
    missVar[2] <- missing(data)
    missVar[3] <- missing(CALL)
    if (any(missVar)) {
        missVarTXT <- paste(c("variable", "data", "CALL")[missVar],
                            collapse = ", ")
        stop(paste("Missing arguments with no default:", missVarTXT))
    }
    # ---------------------------------------------------------------

        print(match.call())

    # Convert input values to character
    variable <- as.character(match.call()$variable)
    data     <- as.character(match.call()$data)

    # Get a value of variable in a call object `CALL`. This value is a
    # name of variable of inerest:
    txt1 <- paste0("Name <- CALL$", variable)
    eval(parse(text = txt1))

    if(!is.null(Name))  {
        Name <- as.character(Name)
        txt2 <- paste0(variable,
                       " <- if(Name %in% colnames(",data,")) ",
                       data,"[,Name] else env$", variable)


        (parent.frame())

        eval(parse(text  =  txt2))
    }

    return(eval(parse(text  =  variable)))
}
