
#' Example dataset #1.
#'
#' A dataset containing example data with variables \code{ID}, \code{gr}
#' and \code{.row}.
#'
#' @format A data frame with ... rows and 3 variables:
#' \describe{
#'   \item{ID}{An identification code.}
#'   \item{gr}{A factor variable with severalgroups.}
#'   \item{.row}{A row number.}
#' }
#' @source Artificially generated in R.
#' @examples
#'    DataSet1 <- data.frame(ID = gl(n = 20, k = 2),
#'                          gr = gl(n = 4, labels = LETTERS[1:4], k = 10))
#'    DataSet1$.row <- 1:nrow(DataSet1)
#'
#'    # devtools::use_data(DataSet1)
"DataSet1"

