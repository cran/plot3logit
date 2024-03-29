% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{convex_comb}
\alias{convex_comb}
\title{Computes convex combinations of two vectors}
\usage{
convex_comb(w, x, y, simplify = TRUE)
}
\arguments{
\item{w}{\code{numeric} in \eqn{[0,1]} (multiple values are allowed).}

\item{x, y}{\code{numeric} vectors of equal length.}

\item{simplify}{if \code{TRUE} a matrix is returned; if \code{FALSE} the
output will be a \code{list}.}
}
\description{
Given two vectors and one or multiple coefficients in \eqn{[0,1]},
convex combinations of vectors are computed.
}
\keyword{internal}
