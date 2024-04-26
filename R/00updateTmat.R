#' Create a SOMI object
#'
#' Updates the matrix of sufficient statistics for an E step for a given missing data pattern
#'
#' @param X A rectangular data set (matrix or data.frame) with named columns.
#' @param wt A vector of weights for each column
#' @param Tmat Original matrix of sufficient statistics to update
#' @param theta Augmented covariance matrix as produced by \code{augmentCov}
#' @param obs   numeric vector indicating of membership of data rows to the missing data pattern
#' @param v_obs character vector of names of fully observed variables in the missing data pattern
#' @param v_mis character vector of names of variables with missing values the missing data pattern
#' @return an updated Tmat (matrix)
#'
#' @author Edoardo Costantini, 2023
#' @examples
#' # Create a data matrix
#' X <- matrix(
#'   data = c(
#'     16, 20, 16, 20, -6, -4, 12, 24, 12, -6, 4, -8,
#'     8, 8, 26, -4, 4, 8, 20, 8, NA, NA, 20, -4,
#'     8, 4, -8, NA, 22, -8, 10, 20, 28, -20, -4, -4,
#'     4, 28, 24, 12, 8, 18, -8, 20, 24, -3, 8, -24,
#'     NA, 20, 24, 8, 12, NA
#'   ),
#'   ncol = 6,
#'   byrow = TRUE,
#'   dimnames = list(NULL, c("15P", "15L", "15H", "90P", "90L", "90H"))
#' )
#'
#' # Create a SOMI object for this matrix
#' SOMI <- createSOMI(X)
#'
#' # Compute starting matrices
#' Tmat <- computeTobs(X, wt = wt, S = SOMI$S, I = SOMI$I)
#' theta <- augmentCov(
#'   covmat = cov(X, use = "complete.obs"),
#'   center = colMeans(X, na.rm = TRUE)
#' ) # current augmented covariance matrix
#'
#' # Update the Tmat with info in the forth missing data pattern
#' updateTmat(
#'   X = X,
#'   wt = wt,
#'   Tmat = Tmat,
#'   theta = theta,
#'   obs = SOMI$I[[4]],
#'   v_mis = SOMI$M[[4]],
#'   v_obs = SOMI$O[[4]]
#' )
#'
#' @export
updateTmat <- function(X, wt = rep(1, ncol(X)),
                       Tmat, theta,
                       obs, v_mis, v_obs) {
  # Extract name of all variables involved
  v_all <- colnames(X)

  # Sweep theta over predictors for this missing data pattern
  theta <- SWP(theta, v_obs)

  # Extract MLR coefficient estimates
  betas <- theta[c("int", v_obs), v_mis]

  # Define expectations
  cjs <- cbind(1, X[obs, v_obs, drop = FALSE]) %*% betas

  # Update Tmat matrix
  for (i in seq_along(obs)) {
    for (j in seq_along(v_mis)) {
      # Update for mean
      J <- which(v_all == v_mis[j])
      Tmat[1, J + 1] <- Tmat[1, J + 1] + cjs[i, j] * wt[obs[i]]
      Tmat[J + 1, 1] <- Tmat[1, J + 1]

      # Update for covariances w/ observed covariates for this id
      # (for Ks observed for this id)
      for (k in seq_along(v_obs)) {
        K <- which(v_all == v_obs[k])
        Tmat[K + 1, J + 1] <- Tmat[K + 1, J + 1] + cjs[i, j] * X[obs[i], K] * wt[obs[i]]
        Tmat[J + 1, K + 1] <- Tmat[K + 1, J + 1]
      }

      # Update for covariances w/ unobserved covariates for this id
      # (both j and k missing, includes covariances with itself k = j)
      for (k in seq_along(v_mis)) {
        K <- which(v_all == v_mis[k])
        if (K >= J) {
          Tmat[K + 1, J + 1] <- Tmat[K + 1, J + 1] + (theta[K + 1, J + 1] +
            cjs[i, j] * cjs[i, k]) * wt[obs[i]]
          Tmat[J + 1, K + 1] <- Tmat[K + 1, J + 1]
        }
      }
    }
  }

  # What to return
  return(Tmat)
}