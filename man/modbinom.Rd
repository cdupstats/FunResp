% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modbinom.R
\name{modbinom}
\alias{modbinom}
\title{Multicategory logit models}
\usage{
modbinom(D, HRavail, model, ...)
}
\arguments{
\item{D}{contains covariates only for the focal habitat type and the baseline category}

\item{HRavail}{a data.frame obtained with HRti}

\item{model}{model formula}
}
\description{
Calculates a given logistic regression models for all habitat types separately except from the baseline category. Is used in function \code{\link{multicat1}}.
It ensures that availability is set correctly for the focal habitat type. HRavail is calculated with HRti.
}
\examples{
# see multicat1
}
\keyword{cats}
