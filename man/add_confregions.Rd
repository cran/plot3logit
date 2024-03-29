% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/conf_region.R
\name{add_confregions}
\alias{add_confregions}
\title{Compute the confidence regions of covariate effects}
\usage{
add_confregions(x, conf = 0.95, npoints = 100)
}
\arguments{
\item{x}{an object of class \code{field3logit} or \code{multifield3logit}.}

\item{conf}{confidence level of the regions.}

\item{npoints}{number of points of the borders of the regions.}
}
\value{
Object of class \code{field3logit} or \code{multifield3logit} with updated
confidence regions.
}
\description{
Given the confidence level, it computes the confidence regions of the effects
for each arrow of the \code{field3logit} or \code{multifield3logit} object given in
input. If the \code{field3logit} or \code{multifield3logit} object already contains the
confidence regions, they will be updated if the value of \code{conf} is different.
}
\details{
Given a reference probability distribution \eqn{\pi_0} over the simplex
\eqn{S=\{(\pi^{(1)}, \pi^{(2)}, \pi^{(3)})\in[0,1]^3\colon \pi^{(1)}+\pi^{(2)}+\pi^{(3)}=1\}},
and a change \eqn{\Delta\in\mathbf{R}^k} of covariate values, the confidence
region of the probability distribution resulting from the covariate change
\eqn{\Delta} is computed by means of the Wald statistics
\insertCite{severini2000}{plot3logit}, which should satisfy the following
condition \insertCite{wooldridge2010}{plot3logit}:
\deqn{
(\delta-\hat\delta)^\top
[(I_2\otimes\Delta)^\top\,\hat\Xi\,(I_2\otimes\Delta)]^{-1}
(\delta-\hat\delta)
\leq\chi^2_2(1-\alpha)
}
where \eqn{\hat\delta=\hat{B}^\top\Delta\in\mathbf{R}^2} is the point
estimate of change of natural parameters associated to \eqn{\Delta},
\eqn{\hat{B}=[\beta^{(2)}, \beta^{(3)}]\in\mathbf{R}^{k\times 2}} is the
matrix of point estimates of regression coefficients, \eqn{I_2} is the
identity matrix of order two, \eqn{\otimes} is the Kronecker product,
\eqn{\hat\Xi\in\mathbf{R}^{2k\times2k}} is the covariance matrix of
\eqn{vec(\hat{B})}, and finally, \eqn{\chi^2_2(1-\alpha)} is the
\eqn{(1-\alpha)} quantile of \eqn{\chi^2_2}.

The set of points which satisfy the previous inequality with equal sign
delimits the border of the confidence region for \eqn{\delta}.

If we denote with \eqn{\mathcal{R}_\delta} the set of points \eqn{\delta}
which satisfy the previous inequality, it is possible to obtain the
confidence region of the effect of the covariate change \eqn{\Delta} over the
simplex \eqn{S} as follows:
\deqn{
\mathcal{R}=\{g^\leftarrow(g(\pi_0)+\delta)\colon \delta\in\mathcal{R}_\delta\}
\subseteq S
}
where \eqn{g\colon S\to\mathbf{R}^2} and
\eqn{g^\leftarrow\colon\mathbf{R}^2\to S} are respectively the link function
of the trinomial logit model and its inverse. They are defined as follows:
\deqn{
g(\pi)= g([\pi^{(1)},\pi^{(2)},\pi^{(3)}]^\top)
=\left[\ln\frac{\pi^{(2)}}{\pi^{(1)}}\,,\quad\ln\frac{\pi^{(3)}}{\pi^{(1)}}\right]^\top
}
\deqn{
g^\leftarrow(\eta)=g^\leftarrow([\eta_2,\eta_3]^\top)
=\left[
\frac{1}{1+e^{\eta_2}+e^{\eta_3}}\,,\quad
\frac{e^{\eta_2}}{1+e^{\eta_2}+e^{\eta_3}}\,,\quad
\frac{e^{\eta_3}}{1+e^{\eta_2}+e^{\eta_3}}
\right]^\top\,.
}
For further details and notation see
\insertCite{santi2022;textual}{plot3logit} and
\insertCite{santi2019;textual}{plot3logit}.
}
\examples{
data(cross_1year)

mod0 <- nnet::multinom(employment_sit ~ gender + finalgrade,
  data = cross_1year)
field0 <- field3logit(mod0, 'genderFemale')
field0
add_confregions(field0)

}
\references{
\insertAllCited{}
}
