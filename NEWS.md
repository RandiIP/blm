---
output:
  md_document
---

# blm

December 11th 2015:
The prior and posterior background functions is implemented. The structure of the blm object is finished.

December 15th 2015:
The methods coefficients, confint, deviance, fitted, plot, predict, print, residuals and summary is implemented.

January 17th 2016:
The plot method is rewritten so it is not necessary to provide a parameter list. If not provided the response and first predictor is extracted from the formula.

January 18th 2016:
The summary function now return an object with the coefficients, confint, deviance, fitted values and residuals, while it still make a printout of the statistics.
