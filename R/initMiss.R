# Project:   lmfa
# Objective: function to initialize the objects we need to work with missing data
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2022-03-01

initMiss <- function (x, S, I){

  # Make sure x is a matrix
  x <- as.matrix(x)

  # Create empty sufficient statistics matrix (Tobs) for all missing data patterns
  Tobs_s <- vector("list", S)

  # Fill all the Tobs in
  for (s in 1:S) {
    # Create augmented covariance matrix
    dat_aug <- cbind(
      int = 1,
      x[I[[s]], , drop = FALSE]
    )
    Tobs_s[[s]] <- crossprod(dat_aug)

    # Fix NAs to 0
    Tobs_s[[s]][is.na(Tobs_s[[s]])] <- 0
  }

  # Sum all the Tobs together
  Tobs <- Reduce("+", Tobs_s)

  # Output
  return(Tobs)
}