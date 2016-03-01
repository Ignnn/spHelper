#' [+] List all functions in a package
#'
#' List all functions in a package and return it as a data frame.
#'
#' @param Package A name of a package. Default \code{Package = "spHelper"}.
#'
#' @return A data frame with function names in a package.
#' @export
#'
#' @examples
#'
#' listFunctions()
#' listFunctions(Package = "tidyr")
#'
listFunctions <- function(Package = "spHelper")   {
    # Main function
    FunctionList <- unclass(lsf.str(envir = asNamespace(Package),
                                    all = TRUE))

    # Annotations
    df <- data.frame(Functions = FunctionList)
    names(df) <- paste0("Functions in ",
                        Package,
                        " (",packageVersion(Package),")")

    # Return
    return(df)
}

