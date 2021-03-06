% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/actualize.R
\name{limits_FUN}
\alias{limits_FUN}
\title{Limit function fit to AI package}
\usage{
limits_FUN(actions_matrix, car, t, ...)
}
\arguments{
\item{actions_matrix}{matrix of acc and steer.}

\item{car}{car list.}

\item{t}{numeric time interval.}

\item{...}{}
}
\value{
list of acc and steer.
}
\description{
Limit function fit to AI package
}
\examples{
limits_FUN(matrix(c(14, 23), ncol=2), set(reset()), 1)
}
