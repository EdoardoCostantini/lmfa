# Project:   lmfa
# Objective: function to create an augmented covariance matrix
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2022-03-01
# Notes:     This function only combines a vector of means and a covariance
#            matrix into an augmented covariance matrix, it does not compute
#            any of the values

augmentCov <- function(covmat, center) {
  # Internals -------------------------------------------------------------

  # covmat = cov(mtcars)      # covariance matrix of a dataset
  # center = colMeans(mtcars) # vector of means of a dataset

  # Body ------------------------------------------------------------------

  # Dimensionality
  p <- ncol(covmat)

  # Define starting values for theta
  augCov <- matrix(rep(NA, (p + 1)^2 ),
                   ncol = (p + 1),
                   dimnames = list(c("int", colnames(covmat)),
                                   c("int", colnames(covmat))
                   ))
  augCov[, 1]   <- c(-1, center) # T1 CC
  augCov[1, ]   <- c(-1, center)
  augCov[-1,-1] <- covmat

  # Output
  return(augCov)
}
