% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qplot_crosstab.R
\name{qplot_crosstab}
\alias{qplot_crosstab}
\alias{qplot_crosstab0}
\alias{qplot_crosstab0s}
\alias{qplot_crosstab_sort}
\title{[!] Plot a crosstabulation (classification table)}
\usage{
qplot_crosstab(tabl, Title = "Cross-tabulation", xLabel = NULL,
  yLabel = NULL, subTitle = NULL, text.size = 5, sort.maxOnDiag = FALSE,
  bg.color = "wheat2", max.color = "gold3", low.color = "grey80",
  decimals = 2, show.max = c("colMax", "rowMax", "max", FALSE),
  shades = TRUE, guide = c("legend", "colorbar", FALSE))

qplot_crosstab_sort(tabl, sort.maxOnDiag = TRUE, show.max = TRUE,
  shades = TRUE, ...)

qplot_crosstab0(tabl, Title = "Cross-tabulation", xLabel = NULL,
  yLabel = NULL, subTitle = NULL, text.size = 5, sort.maxOnDiag = FALSE,
  bg.color = "wheat2", decimals = 2, show.max = FALSE, shades = FALSE,
  guide = FALSE, ...)

qplot_crosstab0s(tabl, Title = "Cross-tabulation", xLabel = NULL,
  yLabel = NULL, subTitle = NULL, text.size = 5, sort.maxOnDiag = TRUE,
  bg.color = "wheat2", decimals = 2, show.max = FALSE, shades = FALSE,
  guide = FALSE, ...)
}
\arguments{
\item{tabl}{A classification table, crosstabulation: either an object of
a class "table" or a square matrix.}

\item{Title}{The main title for the plot.}

\item{xLabel}{A label for abscisa (x) axis.}

\item{yLabel}{A label for ordinate (y) axis.}

\item{subTitle}{The second line of title, which will be smaller and
and in italics.}

\item{text.size}{The size of text inside cells.}

\item{sort.maxOnDiag}{Logical. If \code{TRUE}, function
\code{\link{sortDescOnDiag}} is applied (rows and columns are sorted
in so that maximum values were on diagonal, if possible).}

\item{bg.color}{The main background color (used for high values too).}

\item{max.color}{A background for maximum values.}

\item{low.color}{A background for low values.}

\item{decimals}{The number of decimal positions in rounding. Default is 2
(i.e., precission is 0.01).}

\item{show.max}{The kind of top values to be highlighted. Possible entries:
\code{"colMax"} (maxima of every column), \code{"rowMax"} (maxima of every row),
 \code{"max"} (maximum of whole matrix) or  \code{FALSE} (maxima are not highlighted).}

\item{shades}{Logical. If \code{TRUE}, color of a cell varies depending
not on its value (except the cells with top values if \code{show.max} is
  \code{FALSE}).}

\item{guide}{A type of guide to display: either \code{"legend"},
\code{"colorbar"} or \code{FALSE} (if no guide is needed).}

\item{...}{(The same parameters as above).}
}
\value{
A plot: an object of class \code{\link[ggplot2]{ggplot}}.
       More details in package \pkg{ggplot2}).
}
\description{
Plot a crosstabulation (classification table)
}
\examples{

# Generate data: Random guess  ============================
 N <- 1000 # number of observations

Prediction <- sample(c("A","B","C","D"), N, replace = TRUE)
Reference  <- sample(c("A", "B","C","D","E"),N, replace = TRUE)

tabl <- table(Prediction,Reference)
qplot_crosstab(tabl)
qplot_crosstab_sort(tabl)   # different order of columns and rows
qplot_crosstab0(tabl)       # no colors
qplot_crosstab0s(tabl)      # no colors, different order of columns and rows


}
\author{
Vilmantas Gegzna
}
\seealso{
Other \pkg{spHelper} plots: \code{\link{nTick}},
  \code{\link{plot_colors}}, \code{\link{plot_hy.palette}},
  \code{\link{plot_spCompare}}, \code{\link{plot_spDiff}},
  \code{\link{qplolt_spStat}},
  \code{\link{qplot_confusion}},
  \code{\link{qplot_infoDim}}, \code{\link{qplot_kAmp}},
  \code{\link{qplot_kSp}}, \code{\link{qplot_prediction}},
  \code{\link{rmExpr}}, \code{\link{stat_chull}}
}

