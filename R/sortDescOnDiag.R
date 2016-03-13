#' [!!!] Sort rows and columns of crosstabulation by the best match
#'
#' @param M A matrix.
#'
#' @export
#'
#' @examples
#'
#' M <- matrix(c(2,10,10,10,8,9,4:6,4,1,8),byrow=TRUE,nc=3)
#' M <- pkgmaker::addnames(M)
#'
#' M1 <- sortDescOnDiag(M)
#' qplot_crosstab0(M1)
#'
#'
#' qplot_confusion(sortDescOnDiag(M))
#'
#' @author Vilmantas Gegzna
#' @family matrix operations in \pkg{spHelper}
#'
sortDescOnDiag <- function(M) {
    # Eliminate rows and columns by converting to `NA`
    RC.elim <- function(x) {x[mxRow,] <- NA; x[,mxCol] <- NA; return(x)}

    n    <- min(dim(M))
    iCol <- iRow <- rep(NA, n)
    iM   <- ind.matrix(M)

    P <- prop.table(M, which.min(dim(M))) # table of proportions

    Rows <- row(M)
    Cols <- col(M)
    y <- M

    #In each cycle find best match and eliminate rows and columns of thar match
    for (i in 1:n) {
        ind <- iM[y == max(y, na.rm = T)]
        ind <- ind[!is.na(ind)]

        #if there are several maxima, chose (first) one with greater row/column values
        if (length(ind) > 1) {ind <- ind[which.max(P[ind])][1]}

        mxRow <- Rows[ind]
        mxCol <- Cols[ind]

        iRow[i] = mxRow
        iCol[i] = mxCol

        y <- RC.elim(y)
    }

    iRow <- c(iRow, setdiff(1:nrow(y), iRow))
    iCol <- c(iCol, setdiff(1:ncol(y), iCol))

    M1 <- M[iRow,iCol]

    return(M1)
}

