#' [+] Find rows with outlier scores
#'
#' Return indices of rows in scores matrix that contain outliers. A row is
#' treated as having an oultier if any
#' \href{http://wiki.eigenvector.com/index.php?title=Advanced_Preprocessing:_Variable_Scaling#Autoscale}{autoscaled}
#' score in that row is not between \code{±zLimit}.
#'
#' @template scores
#'
#' @param zLimit A threshold for standardized (autoscaled) values
#' (i.e., \href{https://en.wikipedia.org/wiki/Standard_score}{z-scores})
#'  to be treated as an outlier. If \code{(-zLimit) < scale(scores)} or
#'        \code{scale(scores) > (+zLimit)} it is treated as an outlier.\cr
#'       Default \code{zLimit = 2}.
#'
#' @return Vector of indices that indicate rows containing outliers.
#' @export
#'
#' @examples
#'
whichOutlier <- function(scores, zLimit = 2) {
    SS <- scale(scores);
    iOutlier <- which(apply({(SS < -zLimit) | (SS > zLimit)},1,any))
    return(iOutlier)
}
