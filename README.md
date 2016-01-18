---
output:
  md_document:
    variant: markdown_github
---

# blm
Exam Project for Data Science
   
   
Bayesian Linear Regression package.   
To fit a model using bayesian linear regression call the function blm(formula, data, prior = NULL, alpha = 1, beta, n = 2).   
Through prior can a blm object be provided else the alpha and n must be provided.   
   
The methods coefficients, confint, deviance, fitted, plot, predict, print, residuals and summary is implemented for this kind of object.
