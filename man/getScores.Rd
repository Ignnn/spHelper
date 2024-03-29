% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getScores.R
\name{getScores}
\alias{getScores}
\title{[+] Calculate amplitudes of spectroscopic components (a.k.a. scores) and label the resulting object}
\usage{
getScores(sp, loadings = NULL, xLabel = "Component", yLabel = "Amplitude",
  names.var = "kNames", scores = NULL)
}
\arguments{
\item{sp}{Spectroscopic data
(either a \code{\link[=hyperSpec-class]{hyperSpec}} object or a matrix).}

\item{loadings}{Components' spectra (loadings) as either a matrix or a
\code{\link[=hyperSpec-class]{hyperSpec}} object.}

\item{xLabel}{A label for x axis. Default is "Component".\cr
\code{labels(scores,".wavelength") <- xLabel}}

\item{yLabel}{A label for y axis. Default is "Amplitude".\cr
\code{labels(scores,"spc") <- yLabel}}

\item{names.var}{A name of variable in \code{loadings}, that contains names
of components (loadings). These names will be transfered to
\code{scores}.\cr\bold{NOTES:}\cr
1. if \code{names.var} does not
exist (e.g., misspelled), component names No1, No2, ... will be used. \cr
2. This parameter applicable only if class of \code{sp} is
 \code{hyperSpec}.}

\item{scores}{A matrix of known/already calculated scores to convert to
\code{hyperSpec} object.\cr
If this argument is provided, \emph{matrix multiplication is not
performed,} but component names are copied from \code{loadings} to
\code{scores}.}
}
\value{
Amplitudes of the components (i.e., scores), tha corespond to
observations (spectra) in object \code{sp}.
}
\description{
Calculate amplitudes of spectroscopic components (a.k.a. scores) by
matrix multiplication (see section "Detais") and properly label created
\code{hyperSpec} object.
}
\details{
Equation of matrix multiplication to calculate scores:

\deqn{scores = sp * loadings * inv(loadings' * loadings)}

This formula is taken and adapted from [1].
}
\examples{

sc <- getScores(Spectra, Loadings)
sc
qplot_scores(sc)
}
\author{
Vilmantas Gegzna
}
\references{
[1] M. Brydegaard et al. IEEE Photonics J 2011:3(3);406-21.
}
\seealso{
\code{\link{qplot_scores}}

Other component analysis / factorisation related functions in \pkg{spHelper}: \code{\link{infoDim}},
  \code{\link{plot_spDiff}}, \code{\link{qplot_infoDim}},
  \code{\link{qplot_kAmp}}, \code{\link{qplot_kSp}},
  \code{\link{reconstructSp}}, \code{\link{sortLoadings}},
  \code{\link{unipeak}}, \code{\link{whichOutlier}}
}

