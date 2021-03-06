% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelMethods.R
\name{plot.blm}
\alias{plot.blm}
\title{Plot a blm object}
\usage{
\method{plot}{blm}(x, parm = NULL, ...)
}
\arguments{
\item{x}{the blm object}

\item{parm}{a vector with the name of the responsevariable and the name of the predictor varialbe to be plotted. Only the first two variables in the vector is considered. Default NULL. If not provided the first predictor in the model will be used as predictor.}

\item{...}{any extra parameters}
}
\value{
a x-y plot with the data points and the line from the blm-fit and a residual plot
}
\description{
This function makes plots of a blm object.
}

