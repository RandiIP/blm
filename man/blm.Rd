% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/blmObject.R
\name{blm}
\alias{blm}
\title{Fit the data to a model using bayesian linear regression}
\usage{
blm(formula, data, prior = NULL, alpha = 1, beta, n = 2)
}
\arguments{
\item{formula}{the formula for the model to fit}

\item{data}{the data to fit}

\item{prior}{the prior distribution, default = NULL, if not a blm object provide values for alpha and n}

\item{alpha}{the alpha value, only needed if prior isn't provided, default = 1}

\item{beta}{the beta value}

\item{n}{the number of parameters in the model including the response, only needed if prior isn't provided, default = 2}
}
\value{
an object of class blm with a list containing the call, the fitted posterior, the model, beta and the data.
}
\description{
This function creates a blm object with the fitted model
}

