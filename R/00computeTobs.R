#' Compute T matrix based on observed values
#'
#' Computes a (weighted) matrix of sufficient statistics for means and covariances
#'
#' @param X A rectangular data set (matrix or data.frame) with named columns.
#' @param wt A vector of weights for each column
#' @param S A vector of weights for each column
#' @param I Original matrix of sufficient statistics to update
#' @param dimnames A list of dimension names (if they should be changed from the names of the columns of X)
#' @return A (weighted) matrix with sufficient statistics for the mean on the diagonal elements and for the covariances on the off-diagonal elements
#'
#' @details This function can be used to create the Tobs for each state.
#' The EM algorithm needs a Tobs that is updated at every iteration in the E step.
#' In LMFA, we need a Tobs for every state, as the data is weighed differently based on the state.
#' @author Edoardo Costantini, 2024
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
#' # Sample some weights
#' wt <- runif(nrow(x))
#' 
#' # Compute the T matrix
#' Tobs <- computeTobs(X, wt = wt, S = SOMI$S, I = SOMI$I)
#'
#' @export
computeTobs <- function (X, S, I, wt,
                         dimnames = list(c("int", colnames(X)),
                                         c("int", colnames(X)))
){
  # Create empty storing object
  Tobs_s <- vector("list", S)

  # Compute Tobs for all miss patts
  for(s in 1:S){
    # Define what you are working with in this miss patt
    x_s <- X[I[[s]], , drop = FALSE]
    wt_s <- wt[I[[s]]]

    # Weighted augmented design matrix
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