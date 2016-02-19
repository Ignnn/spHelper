#' @name Datasets-DataSet1
#'
#' @title Dataset: a data frame for illustrations
#'
#' @description A dataset containing example data with variables \code{ID}, \code{gr}
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


#' @name Datasets-Loadings
#' @aliases Datasets-Loadings
#'
#' @title [!] DataSets: Loadings
#'
#' @description A dataset containing Loadings ...
'Loadings'

#' @rdname Datasets-Loadings
'Loadings3'

#' @rdname Datasets-Loadings
'Loadings4'


#' @name Datasets-Scores
#' @aliases Datasets-Scores
#'
#' @title [!] DataSets: Scores
#'
#' @description A dataset containing Scores ...
'Scores'
#' @rdname Datasets-Scores
'Scores3'
#' @rdname Datasets-Scores
'Scores4'

#' @name Datasets-Spectra
#' @aliases Datasets-Spectra
#'
#' @title [!] DataSets: Spectra
#'
#' @description A dataset containing spevtroscopic data ...
'Spectra'
#' @rdname Datasets-Spectra
'Spectra3'
#' @rdname Datasets-Spectra
'Spectra4'
