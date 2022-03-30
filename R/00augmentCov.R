# Project:   lmfa
# Objective: function to create an augmented covariance matrix
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2022-03-30
# Notes:     This function only combines a vector of means and a covariance
#            matrix into an augmented covariance matrix, it does not produce
#            any new estimate.

augmentCov <- function(covmat, center,
                       dnames = list(c("int", colnames(covmat)),
                                       c("int", colnames(covmat)))
) {
  # Internals -------------------------------------------------------------

  # covmat = cov(mtcars)      # covariance matrix of a dataset
  # center = colMeans(mtcars) # vector of means of a dataset
  # dnames = list(c("int", colnames(covmat)),
  #                 c("int", colnames(covmat))) # list of row and column names

  # Body ------------------------------------------------------------------

  # Store the dimensionality of the data
  p <- ncol(covmat)

  # Define the structure of the augmented covariance matrix
  augCov <- matrix(rep(NA, (p + 1)^2 ),
                   ncol = (p + 1),
                   dimnames = dnames)

  # Assign the values to the correct slot
  augCov[1, 1] <- -1
  augCov[-1, 1] <- center
  augCov[1, -1] <- center
  augCov[-1,-1] <- covmat

  # Output
  return(augCov)
}
