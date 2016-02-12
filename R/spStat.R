#'
#' [!] Calculate summary statistic of spectroscopic data by group and for all data
#'
#' Calculate summary statistic spectra in hyperSpec object by group and for all data
#'
#' @note
#' This function is similar to \code{\link[hyperSpec]{aggregate}},
#' just calculates additional statistic for all spectra. This
#' statistic is indicated by level \code{.ALL}) in variable
#' \code{.aggregate}.
#'
#' @param sp A hyperSpec object.
#' @param by A grouping variable (either variable name of \code{sp},
#'              or vector (factor) of length \code{length(sp)}
#' @param FUN A function to apply.
#'
#' @return A hyperSpec object that contain summary statistic. Additional
#'         column \code{.aggregate} is added. In this column levels of
#'         grouping variable are indicated.
#' @export
#'
#' @seealso \code{\link[hyperSpec]{aggregate}}
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
