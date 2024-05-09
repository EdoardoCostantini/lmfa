#' Compute observed data log-likelihood
#'
#'
#'
#' @param dmv The DMV coefficients produced by function DMV
#' @param n Number of observations.
#' @param n_state Number of states.
#' @param pi_k The state-specific loading matrices.
#'
#' @return Observed data log-likelihood
#' 
#' @export
observed_LL <- function(dmv, n, n_state, pi_k) {
    # Create a storing object for the individual log-likelihood contributions
    logli <- c(rep(0, n))

    # Obtain the observed-data loglikelihood.
    for (i in 1:n) {
        for (k in 1:n_state) {
            logli[i] <- logli[i] + pi_k[[k]] * dmv[k, i]
        }
    }

    # Sum the individual log-likelihood contributions
    total_logl <- sum(log(logli))

    # Return the log-likelihood
    return(total_logl)
}