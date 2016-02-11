# listFunctions -------------------------------------------------------
#
#' [!] List all functions in a package.
#'
#' List all functions in a package.
#'
#' @param Package A name of a package. Default \code{Package = "spHelper"}.
#' @param print.table A logical flag indicating if the result should be printed as a table using
#' \code{\link[pander]{pander}}. Default is \code{FALSE}
#'
#' @return A list of functions in a package.
#' @export
#'
#' @importFrom pander pander
#' @examples
#'
#' listFunctions()
#' listFunctions(Package = "tidyr", print.table = F)
#'
listFunctions <- function(Package = "spHelper", print.table = FALSE, plot = FALSE)   {
    FunctionList <- unclass(lsf.str(envir = asNamespace(Package), all = TRUE))
    if (print.table){
        pander::pander(as.data.frame(FunctionList))
        invisible(FunctionList)
    } else return(data.frame(Functions = FunctionList))

    if (plot){
        require(mvbutils)
        require(sna)

        pkgFW <- mvbutils::foodweb(where=paste0("package:", Package), cex=0.7, charlim=60,  plotting=F)
        sna::gplot(pkgFW$funmat, g = 8,
                   jitter = T,
                   # mode = "mds",
                   label.cex = .6,
                   diag=TRUE,
                   vertex.cex=1:2,
                   displaylabels=TRUE,
                   label.bg="gray90")
    }
}
