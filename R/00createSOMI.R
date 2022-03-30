# Project:   lmfa
# Objective: function to create the SOMI object for missing data treatment
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2022-03-30
# Notes:     This function creates a SOMI list:
#            - S: number of missing data patterns
#            - O: list of names of variables observed in each missing data pat
#            - M: list of names of variables missing in each missing data pat
#            - I: list of observation IDs belonging to each missing data pat

createSOMI <- function (x){

  # Internals -------------------------------------------------------------

  # x = matrix(c(0, 1, NA,
  #              1, 2, NA,
  #              NA, 2, 3,
  #              5, NA, NA),
  #            ncol = 3,
  #            byrow = TRUE) # any dataset with missing values

  # Body ------------------------------------------------------------------

  # data dimensionality
  n <- nrow(x)

  # Define Missing data patterns for x
  patts <- mice::md.pattern(x, plot = FALSE)
  R     <- patts[-nrow(patts), - ncol(patts), drop = FALSE]
  R     <- R[, colnames(x), drop = FALSE]
  Rl    <- R == 1

  # Number of missing data patterns
  S <- nrow(R)

  # Columns observed for a given pattern
  O <- apply(R, 1, function(x) {colnames(R)[x == 1]})

  # Columns missings for a given pattern
  M <- apply(R, 1, function(x) {colnames(R)[x == 0]}) #

  # Define I matrices (which obs in which pattern)
  ry <- !is.na(x)
  I <- vector("list", S)
  for (s in 1:S) {
    # s <- 1
    index <- NULL
    for (i in 1:n) {
      # i <- 1
      if(all.equal(ry[i, ], Rl[s, ]) == TRUE) {
        index <- c(index, i)
      }
    }
    I[[s]] <- index
  }

  # Output
  return(list(
    S = S,  # number of missing data patterns
    O = O,  # obs variable ids for each miss pat
    M = M,  # miss variable ids for each miss pat
    I = I   # membership of data rows in missing dat pats
  ))
}

