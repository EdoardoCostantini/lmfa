#' Augment a covariance matrix
#'
#' Adds the vector of means as a column and row to any covariance matrix
#'
#' @param covmat covariance matrix of a dataset
#' @param center numeric vector of means of a dataset
#' @param dnames A list of dimension names (if they should be changed from the names of the columns of covmat)
#' @return The original covariance matrix augmented with the vector of means.
#'
#' @details This function only combines a vector of means and a covariance matrix into an augmented covariance matrix, it does not produce any new estimate.
#' @author Edoardo Costantini, 2024
#' @examples
#' # Run the function
#' augmentCov(
#'   covmat = cov(mtcars), # covariance matrix of a dataset
#'   center = colMeans(mtcars) # vector of means of a dataset
#' )
#'
#' @export
augmentCov <- function(covmat, center,
                       dnames = list(
                         c("int", colnames(covmat)),
                         c("int", colnames(covmat))
                       )) {
  # Store the dimensionality of the data
  p <- ncol(covmat)

  # Define the structure of the augmented covariance matrix
  augCov <- matrix(rep(NA, (p + 1)^2),
    ncol = (p + 1),
    dimnames = dnames
  )

  # Assign the values to the correct slot
  augCov[1, 1] <- -1
  augCov[-1, 1] <- center
  augCov[1, -1] <- center
  augCov[-1, -1] <- covmat

  # Output
  return(augCov)
}
