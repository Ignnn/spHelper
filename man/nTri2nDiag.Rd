% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nTri2nDiag.R
\name{nTri2nDiag}
\alias{nTri2nDiag}
\title{[+] Matrix: length of diagonal when a number of elements in a lower/upper triangle is given}
\usage{
nTri2nDiag(nTri)
}
\arguments{
\item{nTri}{A number of elements in a lower/upper triangle of a matrix (excluding the diagonal).}
}
\value{
A number of rows/columns in a symmetric matrix. May be a fractional number.
}
\description{
Calculate a number of diagonal elements of a symmetric matrix when a number
elements in a lower/upper triangle is given.
}
\details{
Dedived from equation nCoeff = (n^2-n)/2
}
\examples{
nTri2nDiag(10)
nTri2nDiag(6)

}
\author{
Vilmantas Gegzna
}
\seealso{
Other matrix operations in \pkg{spHelper}: \code{\link{corr_vec2mat}},
  \code{\link{ind.matrix}}, \code{\link{nDiag2nTri}},
  \code{\link{new.matrix}}, \code{\link{sortDescOnDiag}},
  \code{\link{which.in}}, \code{\link{which.max.all}},
  \code{\link{which.max.perRow}}
}

