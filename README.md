# RelimpPCR - Relative Importance PCA Regression

## Description
This package performs PCA dimensionality reduction in the context of a linear regression. In most cases, PCA dimensionality reduction is performed independent of the Y values for a regression. This captures the majority of the variance of the X values, but may not actually be the optimal dimensionality reduction solution for a regression against Y.  

An alternative method, optimized for a regression against Y, is to use both PCA and a relative importance measure. This package applies PCA to a given data frame of X values, and then calculates the relative importance of each PCA factor against Y. It outputs ordered factors that are optimized for model fit. By performing dimensionality reduction with this method, an individual can achieve a the same r-squraed value as performing just PCA, but with fewer predictors.

## Installation
In a shell / command prompt build the package with "R CMD build RelimpPCR" (from the directory above "RelimpPCR")
In a shell / command prompt install the package with "R CMD INSTALL RelimpPCR" (from the directory above "RelimpPCR")

## Use
After importing the package, you can use the "RelimpPCR()" function to perform a relative importance PCA regression.
