% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tbl2matrix}
\alias{tbl2matrix}
\title{Convert a tibble to a matrix}
\usage{
tbl2matrix(x, .rownames = NULL)
}
\arguments{
\item{x}{tibble to be converted.}

\item{.rownames}{name of the column (character) of \code{x} containing the row
names of the output matrix.}
}
\description{
Given a \code{tibble} or a \code{data.frame} as input, \code{tbl2matrix} converts it to a
matrix.
}
\keyword{internal}
