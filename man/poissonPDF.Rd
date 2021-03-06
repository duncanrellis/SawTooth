% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distFuncs.R
\name{poissonPDF}
\alias{poissonPDF}
\title{Problem mass function for poisson distribution}
\usage{
poissonPDF(x, mu, var)
}
\arguments{
\item{x}{numeric, quantile}

\item{mu}{numeric, mean}

\item{var}{numeric, variance}
}
\value{
numeric
}
\description{
Poisson PMF, supports fill-rate calculator of \code{\link[=estimateFR]{estimateFR()}}
}
\keyword{internal}
