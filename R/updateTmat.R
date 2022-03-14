# Project:   lmfa
# Objective: function to update sufficient statistics for an EM algorithm
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2022-03-01

updateTmat <- function(x, Tmat, augCov, obs, dvs, ivs) {
  # Internals -------------------------------------------------------------

  # x <- matrix(data = c(16,20,16,20,-6,-4, 12,24,12,-6,4,-8,
  #                      8,8,26,-4,4,8, 20,8,NA,NA,20,-4,
  #                      8,4,-8,NA,22,-8, 10,20,28,-20,-4,-4,
  #                      4,28,24,12,8,18, -8,20,24,-3,8,-24,
  #                      NA,20,24,8,12,NA),
  #             ncol = 6,
  #             byrow = TRUE,
  #             dimnames = list(NULL, c("15P","15L","15H", "90P", "90L", "90H")))
  # Tmat = initMiss(x, S = SOMI$S, I = SOMI$I) # baseline Sufficient statistics matrix to be updated
  # augCov = augmentCov(covmat = cov(x, use = "complete.obs"),
  #                     center = colMeans(x, na.rm = TRUE)) # current augmented covariance matrix
  # SOMI <- createSOMI(x)
  # obs = SOMI$I[[2]]
  # ivs = SOMI$O[[2]] # e.g. observed in a missing data patterns
  # dvs = SOMI$M[[2]] # e.g. missing in a missing data patterns

  # Body ------------------------------------------------------------------
  # Make sure you are working with a matrix
  x <- as.matrix(x)

  # Store the names of the variables
  vars <- colnames(x)

  # Sweep augCov over the predictors
  augCov <- ISR3::SWP(augCov, ivs)

  # Define expectations (individual contributions)
  betas <- augCov[c("int", ivs), dvs]
  cjs <- cbind(1, x[obs, ivs, drop = FALSE]) %*% betas

  # Update T matrix ##
  for(j in seq_along(dvs) ){
    for(i in seq_along(obs)){
      # j <- 1
      J <- which(vars == dvs[j])
      # Update for mean
      Tmat[1, dvs[j]] <- Tmat[1, dvs[j]] + cjs[i, j]
      Tmat[dvs[j], 1] <- Tmat[1, dvs[j]]

      # Update for covariances w/ observed covariates for this id
      # (for Ks observed for this id)
      for(k in seq_along( ivs )){
        # k <- 1
        Tmat[ivs[k], dvs[j]] <- Tmat[ivs[k], dvs[j]] + cjs[i, j] * x[obs[i], ivs[k]]
        Tmat[dvs[j], ivs[k]] <- Tmat[ivs[k], dvs[j]]
      }

      # Update for covariances w/ unobserved covariates for this id
      # (both j and k missing, includes covariances with itself k = j)
      for(k in seq_along( dvs )){
        # k <- 1
        K <- which(vars == dvs[k])
        if(K >= J){
          Tmat[dvs[k], dvs[j]] <- Tmat[dvs[k], dvs[j]] + augCov[dvs[k], dvs[j]] + cjs[i, j] * cjs[i, k]
          Tmat[dvs[j], dvs[k]] <- Tmat[dvs[k], dvs[j]]
        }
      }
    }
  }

  # What to return
  return(Tmat)
}