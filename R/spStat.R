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
#' @param Name_of.by (Do not change this.)
#'
#' @return A hyperSpec object that contain summary statistic. Additional
#'         column \code{.aggregate} is added. In this column levels of
#'         grouping variable are indicated.
#' @export
#'
#' @seealso \code{\link[hyperSpec]{aggregate}}
#'
#' @examples
#' spStat(Spectra, gr)
#' spStat(Spectra, by = gr,         FUN = IQR)
#' spStat(Spectra, by = Spectra$gr, FUN = IQR)
#'
spStat <- function(sp, by, FUN = mean,
                   Name_of.by = as.character(match.call()$by)){
	if (missing(by)) stop('Argument `by` is missing with no default.')

    by <- getVarValues(VAR = by, DATA = sp, CALL = match.call())


    stat_by_gr  <- aggregate(sp, by = by, FUN)
    stat_all    <-     apply(sp, 2,       FUN)

    # Add 2 variables to `stat_all`: 1. `.aggregate`, 2. name of "by":
    stat_all$.aggregate <- factor('.All')
    eval(parse(text = paste0("stat_all$",Name_of.by, " <- factor('.All')")))

    # Drop NAs at this point in order enable `collapse` and
    # to avoid problems with variables of class `POSIXct`
    stat_by_gr <- hyDrop.NA(stat_by_gr)
    stat_all   <- hyDrop.NA(stat_all)

    # Merge the hyperSpec objects:
    sp2 <- hyperSpec::collapse(stat_by_gr, stat_all)

    return(sp2)
}
