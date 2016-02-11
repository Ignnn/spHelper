#' [!] Find rows with outlier scores.
#'
#' Find row numbers in scores matrix that contain outliers \cr\cr
#'
#' A row is treated as having an oultier if any
#' \href{autoscaled}{http://wiki.eigenvector.com/index.php?title=Advanced_Preprocessing:_Variable_Scaling#Autoscale}
#' value of that row is outside ±zScore (**grey** lines in figures below);
#'
#' @param scores ??? a matrix or a hyperSpec object of scores (component amplitudes)
#'        after decompositions to components.
#'
#' @param zScore ... Default zScore = 2.
#'
#' @return Vector of indices indicating rows which contain outliers.
#' @export
#'
#' @examples
#'
whichOutlier <- function(scores, zScore = 2) {
    SS <- scale(scores);
    iOutlier <- which(apply({(SS < -zScore) | (SS > zScore)},1,any))
    return(iOutlier)
}
