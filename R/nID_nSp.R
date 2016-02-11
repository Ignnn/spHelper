
#' [!] Calculate number and percentage of specimens and their spectra.
#'
#' Calculate number and percentage of specimens and their spectra. number(spectra) >= number(specimens)
#'
#' data     - a data frame with variables denoted by \code{ID} and \code{gr}
#' ID, gr   - varable names in \code{data} with variables, tad contain IDs and group names (factor levels) respectively.
#'
#'
#' @param data A data frame with variables, that names are denoted by \code{ID} and \code{gr}
#' @param ID ...
#' @param gr ...
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
#'
#' nID_nSp(data, ID, gr)
#' pander::pander(nID_nSp(data, ID, gr))
#'
#' # For hyperSpec object
#' nID_nSp(Spectra$.., ID, gr)
#'
nID_nSp <- function(data, ID, gr){



    percents <- function(TABLE) {paste0(round(prop.table(TABLE),3)*100,"%")}

    nID <- table(unique(data[,c("ID", "gr")])[ ,"gr"]) # Number of unique medical samples per grpup
    nSp <- table(data[ ,"gr"]) # Number of spectra per group


    tbl <- rbind(nID, percents(nID),  nSp, percents(nSp))

    rownames(tbl) <- c("Number of medical specimens",
                       "Percentage of medical specimens",
                       "Number of spectra",
                       "Percentage of spectra")
    return(tbl)
}
