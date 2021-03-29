#' Summary statistics for step3()
#'
#'
#'
#'
#'
#'
#' @param object An object storing output from the main function Step3()
#' @param ... Further arguments for the default S3 summary method
#' @examples
#' \dontrun{
#' summary(results3)
#' }
#' @export
summary.lmfa_step3 <- function(object, ...){
  if(object$convergence==1){
    cat("\n")
    cat(paste("Estimation converged after",round(object$seconds,2),"seconds.","\n"))
  }else{
    cat("\n")
    cat(paste("Maximum number of iterations reached without convergence.","\n"))
    cat("\n")
  }
  cat("\n")
  cat(paste("LL",round(object$LL,4),sep=" = "),"\n")
  cat("\n")

  cat("\n")
  print(object$WaldTests)

  cat("\n")
  cat(paste("Information about the regression parameter estimates:"),"\n")
  cat(paste("For the initial state probabilities, state 1 is the "),"\n")
  cat(paste("reference category. The transition intensities are "),"\n")
  cat(paste("sorted by rows of the transition matrix and the staying "),"\n")
  cat(paste("rates serve as references, respectively."),"\n")
  cat("\n")
  print(object$estimates)
  cat("\n")
}