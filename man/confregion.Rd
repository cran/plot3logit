% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf_region.R
\name{confregion}
\alias{confregion}
\title{It computes the confidence region in the ternary space}
\usage{
confregion(mu, Sig, conf = 0.95, npoints = 100)
}
\arguments{
\item{mu}{centre of the ellipse.}

\item{Sig}{covariance matrix of the ellipse.}

\item{conf}{confidence level of the region.}

\item{npoints}{number of points of the border.}
}
\value{
\code{data.frame} with three columns (named \code{p1}, \code{p2}, and \code{p3})
with ternary coordinates of the points of the ellipse.
}
\description{
Given the parameters of the confidence ellipse in the space of covariate
coefficients, it returns the confidence region of the effect.
}
\keyword{internal}
