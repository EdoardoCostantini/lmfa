# Project:   lmfa
# Objective: function to create internal objects for the EM missing data treat
# Author:    Edoardo Costantini
# Created:   2022-03-01
# Modified:  2022-03-01

createSOMI <- function (x){

  # data dimensionality
  n <- nrow(x)

  # Define Missing data patterns for x
  patts <- mice::md.pattern(x, plot = FALSE)
  R <- patts[-nrow(patts), - ncol(patts), drop = FALSE]
  R <- R[, colnames(x), drop = FALSE]
  R_logi <- R == 1 # R stored as True and False values

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
      if(all.equal(ry[i, ], R_logi[s, ]) == TRUE) {
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

