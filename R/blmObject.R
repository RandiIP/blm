#' Fit the data to a model using bayesian linear regression
#'
#' This function creates a blm object with the fitted model
#'
#' @param formula the formula for the model to fit
#' @param data the data to fit
#' @param prior the prior distribution, default = NULL, if not a blm object provide values for alpha and n
#' @param alpha the alpha value, only needed if prior isn't provided, default = 1
#' @param beta the beta value
#' @param n the number of parameters in the model including the response, only needed if prior isn't provided, default = 2
#' @return an object of class blm with a list containing the call, the fitted posterior, the model, beta and the data.
#'
#' @export
blm <- function(formula, data, prior = NULL, alpha = 1, beta, n = 2) {
  if (is.null(prior)) { # make prior if not provided
    prior <- make_prior(alpha,n)
  } else if (class(prior) == "blm") { # extract posterior as prior if blm object is provided
    dist = prior
    prior = dist$posterior
  } else {
    stop("invalid 'prior' given, class should be blm, else provide n and alpha")
  }

  posterior = fit_model(formula, data, prior, beta)

  structure(list(call = sys.calls(),
                 posterior = posterior,
                 model = formula,
                 beta = beta,
                 data = data,
                 residualDF = dim(data)[1] - (dim(data)[2] - 1) - 1),
            class = "blm")
}
