#' @name nID_nSp
#' @aliases nID_nSp
#' @aliases nID_nObs
#' @title Calculate number and percentage of unique IDs and observations
#'
#'
#' @description Calculate number and percentage of unique IDs and observations.\cr
#'
#' number(IDs) >= number(observations)
#'
#'
#' @param data A data frame with variables, that names are denoted by \code{ID} and \code{gr}
#' @param ID Either a varable or a variable name in \code{data} with vector of speciment IDs.
#' @param gr Either a varable or a variable name in \code{data} with vector of speciment groups.
#'
#' @param decimals A number of maximum decimal places in percentages.
#'  This parameter is passed to function \code{\link[base]{round}}.
#'
#' @return Table (data frame) with count and percentages.
#' @export
#'
#' @examples
#'
#' nID_nSp(DataSet1, ID, gr)
#' nID_nObs(DataSet1, ID, gr)
#' pander::pander(nID_nSp(DataSet1, ID, gr))
#'
#' # For hyperSpec object
#' nID_nSp(Spectra$.., ID, gr)
#'
nID_nSp <- function(data, ID, gr,
                    ID_text          = "medical specimens",
                    observation_text = "spectra",
                    decimals = 0){

    # Parse input and prepare data ===========================================
    CALL <- match.call()
    if (!is.null(CALL$data)){ # if `data` is provided:
        ID <- getVarValues(ID, data, CALL)
        gr <- getVarValues(gr, data, CALL)
    }
    # -----------------------------------------------------------------------
    if (length(ID)!=length(gr)) stop("Length of `ID` and `gr` must agree.")
    # -----------------------------------------------------------------------
    data <- data.frame(ID = ID, gr = gr)

    # Calculations ===========================================
    percents <- function(TABLE) {tbl <- prop.table(TABLE)*100 %>% round(0);
    paste0(tbl, "%")}

    nID <- table(unique(data[,c("ID", "gr")])[ ,"gr"]) # Number of unique medical samples per grpup
    nSp <- table(data[ ,"gr"]) # Number of spectra per group


    tbl <- rbind(nID, percents(nID),  nSp, percents(nSp))

    rownames(tbl) <- c(paste("Number of", ID_name),
                       paste("Percentage of", ID_name),
                       paste("Number of", observation_text),
                       paste("Percentage of", observation_text)
                       )
    return(tbl)
}

#' @rdname nID_nSp
#' @export

nID_nObs <- function(..., ID_text = "unique ID",
                     observation_text = "observations"){
    nID_nSp(..., ID_text = ID_text,  observation_text = observation_text)
}
