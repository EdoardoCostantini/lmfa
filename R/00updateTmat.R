# Project:   lmfa
# Objective: function to update sufficient statistics for an EM algorithm
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2023-01-26
# Notes:     For a given missing data patters, this function updates the Tmat
#            with weighted expected contributions from the missing cases.

updateTmat <- function(x, wt = rep(1, ncol(x)),
                       Tmat, theta,
                       obs, v_mis, v_obs) {
  # Internals -------------------------------------------------------------

  # x    = matrix(data = c(16,20,16,20,-6,-4, 12,24,12,-6,4,-8,
  #                        8,8,26,-4,4,8, 20,8,NA,NA,20,-4,
  #                        8,4,-8,NA,22,-8, 10,20,28,-20,-4,-4,
  #                        4,28,24,12,8,18, -8,20,24,-3,8,-24,
  #                        NA,20,24,8,12,NA),
  #               ncol = 6,
  #               byrow = TRUE,
  #               dimnames = list(NULL, c("15P","15L","15H", "90P", "90L", "90H")))
  # SOMI  = createSOMI(x)
  # wt    = runif(nrow(x))
  # Tmat  = computeTobs(x = x, wt = wt, S = SOMI$S, I = SOMI$I)
  # theta = augmentCov(covmat = cov(x, use = "complete.obs"),
  #                    center = colMeans(x, na.rm = TRUE)) # current augmented covariance matrix
  # obs   = SOMI$I[[2]]
  # v_obs = SOMI$O[[2]] # e.g. observed in a missing data patterns
  # v_mis = SOMI$M[[2]] # e.g. missing in a missing data patterns

  # Body ------------------------------------------------------------------
  # Extract name of all variables involved
  v_all <- colnames(x)

  # Sweep theta over predictors for this missing data pattern
  theta <- SWP(theta, v_obs)

  # Extract MLR coefficient estimates
  betas <- theta[c("int", v_obs), v_mis]

  # Define expectations
  cjs <- cbind(1, x[obs, v_obs, drop = FALSE]) %*% betas

  # Update Tmat matrix
  for(i in seq_along(obs)){
    for(j in seq_along( v_mis ) ){

      # Update for mean
      J <- which(v_all == v_mis[j])
      Tmat[1, J+1] <- Tmat[1, J+1] + cjs[i, j] * wt[obs[i]]
      Tmat[J+1, 1] <- Tmat[1, J+1]

      # Update for covariances w/ observed covariates for this id
      # (for Ks observed for this id)
      for(k in seq_along( v_obs )){
        K <- which(v_all == v_obs[k])
        Tmat[K+1, J+1] <- Tmat[K+1, J+1] + cjs[i, j] * x[obs[i], K] * wt[obs[i]]
        Tmat[J+1, K+1] <- Tmat[K+1, J+1]
      }

      # Update for covariances w/ unobserved covariates for this id
      # (both j and k missing, includes covariances with itself k = j)
      for(k in seq_along( v_mis )){
        K <- which(v_all == v_mis[k])
        if(K >= J){
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