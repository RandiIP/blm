#' The coefficients of a blm object
#'
#' This function calculates the coefficients of a blm object
#'
#' @param object the blm object
#' @param ... any extra parameters
#' @return the estimated parameters for the fitted model
#'
#' @export
coef.blm <- function(object, ...) object$posterior$mean[,1]

#' The confidence intervals of a blm object
#'
#' This function calculates the confidence intervals of all the estimates in a blm object
#'
#' @param object the blm object
#' @param parm a vector of parameter names or a numeric vector with the wanted parameters
#' @param level the desired confidence level, default = 0.95
#' @param ... any extra parameters
#' @return a matrix with all the confidence intervals for the estimates
#'
#' @export
confint.blm <- function(object, parm, level = 0.95, ...) {
  coefs = coef(object)
  name = names(coefs)

  if (missing(parm)) {
    parm = name
  } else if (is.numeric(parm)) {
    parm = name[parm]
  }

  lvl = (1-level)/2

  ci = matrix(0, nrow = length(parm), ncol = 2, dimnames = list(parm, c(paste(lvl*100, "%", sep=" "), paste((1-lvl)*100, "%", sep=" "))))

  for (i in seq_along(parm)) {
    m = object$posterior$mean[,1][i]
    s = object$posterior$covar[i,i]
    ci[i,1] = qnorm(lvl, mean=m, sd=s)
    ci[i,2] = qnorm((1-lvl), mean=m, sd=s)
  }

  ci
}

#' The deviance of a blm object
#'
#' This function calculates the deviance of a blm object
#'
#' @param object the blm object
#' @param ... any extra parameters
#' @return the deviance
#'
#' @export
deviance.blm <- function(object, ...) {
  sum(residuals(object)**2)
}

#' The predicted response of a blm object
#'
#' This function calculates the predicted response of a blm object
#'
#' @param object the blm object
#' @param newdata the new data to predict response for, if not provided the response is predicted for the data used to fit the model
#' @param ... any extra parameters
#' @return the predicted responses
#'
#' @export
predict.blm <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    newdata = as.data.frame(object$data[,-1])
    if (dim(object$data)[2] == 2) {
      colnames(newdata) = names(object$data)[2]
    }
  }

  predict_response(newdata, object$posterior, object$model, object$beta)
}

#' The fitted response of a blm object
#'
#' This function calculates the response the model given the data used to fit the model
#'
#' @param object the blm object
#' @param ... any extra parameters
#' @return the fitted responses
#'
#' @export
fitted.blm <- function(object, ...) predict.blm(object)

#' The residuals of a blm object
#'
#' This function calculates the residuals of a blm object
#'
#' @param object the blm object
#' @param ... any extra parameters
#' @return the residuals for the fitted data
#'
#' @export
residuals.blm <- function(object, ...) {
  observed = object$data[,1]
  predicted = predict.blm(object)$mean
  observed-predicted
}

#' Plot a blm object
#'
#' This function makes plots of a blm object.
#'
#' @param x the blm object
#' @param parm a vector with the name of the responsevariable and the name of the predictor varialbe to be plotted. Only the first two variables in the vector is considered. Default NULL. If not provided the first predictor in the model will be used as predictor.
#' @param ... any extra parameters
#' @return a x-y plot with the data points and the line from the blm-fit and a residual plot
#'
#' @export
plot.blm <- function(x, parm = NULL, ...) {
  if (is.null(parm)) {
    parm = c(all.vars(x$model)[1],all.vars(x$model)[2])
  }

  devAskNewPage(TRUE)
  response = unlist(x$data[parm[1]])
  predictor = unlist(x$data[parm[2]])

  intercept = coef.blm(x)[1]
  slope = coef.blm(x)[parm[2]]

  plot(predictor, response, xlim = c(min(predictor)-1,max(predictor)+1), ylim = c(min(response)-1,max(response)+1), xlab = parm[2], ylab = parm[1], main = paste("Scatterplot of ", parm[1], " as a function of ", parm[2], "\nplotted with the fitted blm regression line", sep=""))
  abline(a = intercept, b = slope, col = "red")

  residual = residuals.blm(x)

  if (length(labels(terms(x$model))) == 1) {
    fit = predictor
    xlabel = parm[2]
  } else {
    fit = fitted.blm(x)$mean
    xlabel = "Fitted"
  }

  plot(x = fit, y = residual, xlim = c(min(fit)-0.2,max(fit)+0.2), ylim = c(min(residual)-0.2,max(residual)+0.2), xlab = xlabel, ylab = "Residuals", main = "Residualplot")
  abline(h=0, col = "darkgray", lty = 2)
}

#' Print a blm object
#'
#' This function prints the most general information from the object
#'
#' @param x the blm object
#' @param ... any extra parameters
#' @return a print with the call and coefficients
#'
#' @export
print.blm <- function(x, ...) {
  cat(paste("Call:", "\n", x$call, "\n", "\n", "Coefficients:", "\n", sep=""))
  print(coef.blm(x))
}

#' Print a summary of a blm object
#'
#' This function prints the more extensive information from the object
#'
#' @param object the blm object
#' @param ... any extra parameters
#' @return a print with the call, a summary of the residuals, the coefficients, the residual standard error, the R-squared and adjusted R-squared and an object with the coefficients, confint, deviance, fitted values and the residuals.
#'
#' @export
summary.blm <- function(object, ...) {
  resi = residuals.blm(object)
  RSS = sum(resi**2)
  TSS = sum((unlist(object$data[1])-mean(unlist(object$data[1])))**2)
  RSE = sqrt(1/(dim(object$data)[1]-(dim(object$data)[2]-1)-1) * RSS)
  R2 = 1 - RSS/TSS
  AR2 = 1 - (RSS/(dim(object$data)[1]-(dim(object$data)[2]-1)-1))/(TSS/(dim(object$data)[1]-1))
  resi_stat = c("Min" = min(resi), "1Q" = quantile(resi, 0.25, names=FALSE), "Median" = median(resi), "3Q" = quantile(resi, 0.75, names=FALSE), "Max" = max(resi))

  cat(paste("Call:\n", object$call, "\n", "\nResiduals:\n", sep=""))
  print(resi_stat)
  cat(paste("\n", "Coefficients:\n", sep=""))
  print(coef.blm(object))
  cat(paste("\n", "Residual standard error: ", RSE, ", with ", object$residualDF, " degrees of freedom", sep=""))
  cat(paste("\n", "R-squared: ", R2, ", Adjusted R-squared ", AR2, sep=""))

  return(invisible(structure(list(coefficients = coef(object),
                                  confint = confint(object),
                                  deviance = deviance(object),
                                  fitted = fitted(object),
                                  residuals = residuals(object)),
                             class = "summary.blm")))
}

