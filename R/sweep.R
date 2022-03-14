#' Sweep operator
#'
#' This function implements the sweep operator described by Goodnight 1979, p. 154.
#'
#' @param A      Symmetric matrix
#' @param target Integer vector indicating which columns of A should be swept in.
#' @return The H matrix resulting from the sweeping: H = SWP[target]A
#' @details
#' Some details on what this function does.
#' @author Edoardo Costantini, 2021

sweep <- function (A, target){
  for(k in target){
    # Step 1: Let D = a_kk
    D <- A[k, k]

    # Step 2: Divide row k by D.
    A[k, ] <- A[k, ] / D

    # Step 3:
    # - For every other row i != k, let B = a_ik
    # - Subtract B \times row k from row i.
    # - set a_ik = -B/D.
    for(i in 1:seq_along(A)){
      if(i != k){
        B <- A[i, k]
        A[i, ] <- A[i, ] - B * A[k, ]
        A[i, k] <- -1 * B / D
      }
    }
    # Step 4: Set a_kk = 1/D
    A[k, k] <- 1/D
  }

  # Output
  return(A)
}
