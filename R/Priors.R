#' Make the prior distribution
#'
#' This function makes a prior distribution given the alpha and number of parameters in the model
#'
#' @param alpha the alpha value
#' @param n the number of parameters in the model
#' @return a list with a vector of means and a covariance matrix for the prior
make_prior <- function(alpha, n) {
  m = matrix(rep(0,n), nrow= n)
  covar=diag(1/alpha, nrow = n)
  if (n == 2) rownames(covar) = colnames(covar) = rownames(m) = c("(Intercept)","x")
  list(mean=m,covar=covar)
}
