#'
#' [!] Calculate summary statistic by group and for all data
#'
#' @param sp - hyperSpec object.
#' @param  by - grouping variable (either variable name of \code{sp},
#'              or vector (factor) of length \code{length(sp)}
#' @param FUN - function to apply.
#'
#' @return
#' @export
#'
#' @examples
#'
#' spStat(sp, by=sp$gr, FUN = IQR)
#' spStat(sp, by=gr,    FUN = IQR)
#'
spStat <- function(sp, by = gr, FUN = mean){
    varName <- as.character(match.call()$by)
    by <- if (varName %in% colnames(sp)) sp[[,varName]] else by


    stat_by_gr  <- aggregate(sp, by = by, FUN)
    stat_all    <-     apply(sp, 2,       FUN)
    stat_all$.aggregate <- factor(".All")
    sp2 <- collapse(stat_by_gr, stat_all)

    sp2 <- dropCol(sp2)
    return(sp2)
}
