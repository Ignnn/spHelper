#' [!] Find rows with outlier scores.
#'
#' Find row numbers in scores matrix that contain outliers \cr\cr
#'
#' A row is treated as having an oultier if any
#' \href{autoscaled}{http://wiki.eigenvector.com/index.php?title=Advanced_Preprocessing:_Variable_Scaling#Autoscale}
#' value of that row is outside ±zScore (**grey** lines in figures below);
#'
#' @template scores
#'
#' @param zScore A threshold for standardized values to be treated as outlier.
#'       If \code{-zScore < scale(scores)} or \code{scale(scores) >zScore}
#'       it is treated as an outlier.\cr
#'       Default \code{zScore = 2}.
#'
#' @return Vector of indices indicating rows containing outliers.
#' @export
#'
#' @examples
#'
whichOutlier <- function(scores, zScore = 2) {
    SS <- scale(scores);
    iOutlier <- which(apply({(SS < -zScore) | (SS > zScore)},1,any))
    return(iOutlier)
}
