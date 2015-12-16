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

#' Draw a sample from a prior distribution for a linear model
#'
#' This function make sample from the prior distribution
#'
#' @param n the wanted number of samples
#' @param alpha the alpha value
#' @return a matrix with the estimated weights w0 and w1 as columns
#'
#' @importFrom MASS mvrnorm
sample_from_prior <- function(n,alpha) {
  sample = MASS::mvrnorm(n = n, mu = c(0,0), Sigma = diag(1/alpha, nrow = 2))
  colnames(sample) = c("w0","w1")
  rownames(sample) = seq(1,n,1)
  sample
}
