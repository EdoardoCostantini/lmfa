#' Compute Multivariate Normal Densities
#'
#' Needed to calculate the posterio-state membership probabilities and the loglikelihood.
#' The function is based on code in dMvn() that is used internally by the mixture of normals model in bayesm.
#'
#' @param x The dataset.
#' @param n_sub Number of observations. #subject or #total observations.
#' @param Lambda_k The state-specific loading matrices.
#' @param Psi_k The state-specific unique variances.
#' @param n_state Number of states. Has to be a scalar.
#' @param J Number of items.
#' @param nu_k The state-specific intercepts.
#'
#' @return Returns the DMV coefficients.
#'
#' @noRd

DMV <- function(x, Lambda_k, Psi_k, n_state, J, n_sub, nu_k) {
  # Create an empty list to store covariance matrices for every state
  Sigma_k <- rep(list(NA), n_state)

  # Loop over the states to update every Sigma
  for (k in 1:n_state) {
    Sigma_k[[k]] <- (tcrossprod(Lambda_k[[k]]) + Psi_k[[k]])
  }

  # Create an empty matrix to store the densities
  saveDMV <- matrix(NA, nrow = n_state, ncol = n_sub) # empty matrix

  # Loop over the observations
  for (i in 1:n_sub) {
    # Loop over the states
    for (k in 1:n_state) {
      saveDMV[k, i] <- NPflow::mvnpdfC(
        x = as.matrix(unlist(x[i, ])),
        mean = nu_k[[k]],
        varcovM = Sigma_k[[k]],
        Log = FALSE
      )
    }
  }

  # Make DMV a list for convenient storage.
  DMV_list <- rep(list(NA), n_state)
  for (k in 1:n_state) {
    DMV_list[[k]] <- saveDMV[k, ]
  }

  # Return both the matrix and the list version
  return(
    list(
      mat = saveDMV,
      lis = DMV_list
    )
  )
}
