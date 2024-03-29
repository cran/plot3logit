% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ggtern.R
\name{gg3logit}
\alias{gg3logit}
\title{Create a new gg3logit}
\usage{
gg3logit(data = NULL, mapping = aes(), ...)
}
\arguments{
\item{data}{a \code{field3logit} object, a \code{multifield3logit} object, or
a \code{data.frame} structured like a fortified \code{field3logit} or a
\code{multifield3logit} object.}

\item{mapping}{list of aesthetic mappings to be used for plot. If a
\code{field3logit} or a \code{multifield3logit} is passed to \code{data}, none of the
aesthetics mappings listed in section \emph{Aesthetic mappings} below has to be
specified (if specified, they will be overwritten).}

\item{...}{additional arguments passed through to \code{\link[ggtern:ggtern]{ggtern}}.}
}
\value{
Object of class \code{ggplot}.
}
\description{
\code{gg3logit} initialises a \code{\link[ggplot2:ggplot]{ggplot}} object through
\code{\link[ggtern:ggtern]{ggtern}}. If a \code{field3logit} or a \code{multifield3logit}
object is passed to argument \code{data}, the mandatory aesthetics of the ternary
plot are automatically set. See \insertCite{santi2022;textual}{plot3logit}
for details and examples.
}
\section{Aesthetic mappings}{


The following aesthetics are required by at least one of the available stats.
None of them should be specified if a \code{field3logit} or a \code{multifield3logit}
is passed to the argument \code{data} of \code{\link[=gg3logit]{gg3logit()}}, \code{\link[=stat_field3logit]{stat_field3logit()}} or
\code{\link[=stat_conf3logit]{stat_conf3logit()}}:
\itemize{
\item \code{x}, \code{y}, \code{z} are required by:
\itemize{
\item \code{\link[=stat_field3logit]{stat_field3logit()}} as ternary coordinates of the starting points of the
arrows;
\item \code{\link[=stat_conf3logit]{stat_conf3logit()}} ternary coordinates of the points on the border of
confidence regions;
}
\item \code{xend}, \code{yend}, \code{zend}: required by \code{\link[=stat_field3logit]{stat_field3logit()}} as ternary
coordinates of the ending points of the arrows;
\item \code{group}: identifier of groups of graphical objects (arrows and their confidence
regions);
\item \code{type}: type of graphical object (arrows or confidence regions).
}

The following variables of a fortified \code{field3logit} or a \code{multifield3logit}
object may be useful for defining other standard aesthetics (such as \code{fill},
\code{colour}, ...):
\itemize{
\item \code{label} identifies a field through a label, thus it is useful for
distinguishing the fields in a \code{multifield3logit} object.
\item \code{idarrow} identifies each group of graphical objects (arrows and their
confidence regions) \emph{within} every field. Unlike variable \code{group},
\code{idarrow} is not a global identifier of graphical objects.
}
}

\examples{
\donttest{
data(cross_1year)

mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade, data = cross_1year)
field0 <- field3logit(mod0, 'genderFemale')

gg3logit(field0) + stat_field3logit()
}

}
\seealso{
Other gg functions: 
\code{\link{autoplot.Hfield3logit}()},
\code{\link{stat_3logit}()},
\code{\link{stat_conf3logit}()},
\code{\link{stat_field3logit}()}
}
\concept{gg functions}
