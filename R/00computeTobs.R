# Project:   lmfa
# Objective: Computes the weighted Tobs
# Author:    Edoardo Costantini
# Created:   2022-03-15
# Modified:  2022-03-30
# Notes:     This function can be used to create the Tobs for each state.
#            The EM algorithm needs a Tobs that is updated at every iteration
#            in the E step.
#            In LMFA, we need a Tobs for every state, as the data is weighed
#            differently based on the state.

computeTobs <- function (x, S, I, wt,
                         dimnames = list(c("int", colnames(x)),
                                         c("int", colnames(x)))
){
  # Internals -------------------------------------------------------------

  # x = x           # a dataset with missing values
  # S = SOMI$S      # the number of missing data patterns in x
  # I = SOMI$I      # list of the observations ids in every missing data pattern
  # wt = z_ik[[sc]] # vector of weights
  # dimnames = list(c("int", colnames(x)),
  #                 c("int", colnames(x))) # list of row and column names

  # Body ------------------------------------------------------------------

  # Create empty storing object
  Tobs_s <- vector("list", S)

  # Compute Tobs for all miss patts
  for(s in 1:S){
    # Define what you are working with in this miss patt
    x_s <- x[I[[s]], , drop = FALSE]
    wt_s <- wt[I[[s]]]

    # Weigthed augmented design matrix
    dat_aug <- as.matrix(sqrt(wt_s) * cbind(1, x_s))

    # Obtain matrix of sufficient statistics (Tobs) w/ cross-product shortcut
    Tobs_s[[s]] <- t(dat_aug) %*% dat_aug

    # Replace NAs with 0 contributions
    Tobs_s[[s]][is.na(Tobs_s[[s]])] <- 0
  }

  # Sum Tobs_s
  Tobs <- Reduce("+", Tobs_s)

  # Give names to dimensions
  dimnames(Tobs) <- dimnames

  # Output
  return(Tobs)
}