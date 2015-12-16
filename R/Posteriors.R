#' Make a model matrix
#'
#' This function make a model matrix for the data
#'
#' @param model the model the data will be fitted to
#' @param data the data
#' @return a model matrix
make_model_matrix <- function(model, data) {
  model.matrix(model, model.frame(model, data))
}

#' Make a responseless model matrix
#'
#' This function make a responseless model matrix for the data.
#'
#' @param model the model the data will be fitted to
#' @param data the data without a response variable
#' @return a model matrix
make_responseless_matrix <- function(model, data) {
  responseless = delete.response(terms(model))
  make_model_matrix(responseless, data)
}

#' Fit the model
#'
#' This function fits the model to the data given the prior distribution and the beta.
#'
#' @param model the model to fit
#' @param data the data with response
#' @param prior the prior distribution of model
#' @param beta the beta value
#' @return a list with means for each parameter and a covariance matrix
fit_model <- function(model, data, prior, beta) {
  modelMatrix = make_model_matrix(model,data)
  response = model.response(model.frame(model, data))
  s = solve(prior$covar + beta * t(modelMatrix) %*% modelMatrix)
  m = (beta * s) %*% t(modelMatrix) %*% response
  list(mean = m, covar = s)
}

#' Predicting responses
#'
#' This function predict responses given new predictor values and the mean and covariance for the fitted model
#'
#' @param new_data the new values for the predictor variables
#' @param fitted the fitted model, as outputted from fit_model
#' @param model the model from the fit
#' @param beta the beta value
#' @return a list with a vector of means (predicted responses) and a vector of corresponding variances
predict_response <- function(new_data, fitted, model, beta) {
  modelMatrix = make_responseless_matrix(model, new_data)
  mu = apply(modelMatrix, 1, function(predictor) t(fitted$mean) %*% predictor)
  variance = apply(modelMatrix, 1, function(predictor) 1/beta + t(predictor) %*% fitted$covar %*% predictor)
  list(mean = mu, var = variance)
}
