#' Create a SOMI object
#'
#' \code{createSOMI} stores the names of the fully observed variables, the names of the variables with missing values, the identifiers of the data rows belonging to each missing data pattern, for each missing data pattern in the input dataset.
#'
#' @param X A rectangular data set (matrix or data.frame) with named columns.
#' @return
#' Object of class \code{somi}, which is a list containing:
#'  \itemize{
#'    \item \code{S}: unit integer vector storing the number of missing data patterns
#'    \item \code{O}: list of names of fully observed variables in each missing data pattern
#'    \item \code{M}: list of names of variables with missing values in each missing data pattern
#'    \item \code{I}: list of membership of data rows in each missing data pattern
#' }
#' 
#' @author Edoardo Costantini, 2023
#' @examples
#' # Set a seed
#' set.seed(100)
#'
#' # Define an input matrix with simple known missing data
#' data_matrix <- matrix(
#'   c(
#'     0, 1, NA,
#'     1, 2, NA,
#'     NA, 2, 3,
#'     5, NA, NA
#'   ),
#'   ncol = 3,
#'   dimnames = list(NULL, paste0("x", 1:3)),
#'   byrow = TRUE
#' )
#'
#' # Use the function
#' SOMI_object <- createSOMI(X = data_matrix)
#' @export
createSOMI <- function(X) {
  # Store the number of rows in the data matrix
  n <- nrow(X)
  p <- ncol(X)

  # Define missing data patterns for X
  patts <- mice::md.pattern(X, plot = FALSE)
  R <- patts[-nrow(patts), -ncol(patts), drop = FALSE]

  # Name the missing data patterns
  rownames(R) <- paste0("S", 1:nrow(R))

  # Order the variables based on the original order
  R <- R[, colnames(X), drop = FALSE]

  # Create a representation with booleans
  Rl <- R == 1

  # Number of missing data patterns
  S <- nrow(R)

  # Columns observed for a given pattern
  O <- apply(R, 1, function(i) {colnames(R)[i == 1]}, simplify = FALSE)

  # Columns missings for a given pattern
  M <- apply(R, 1, function(i) {colnames(R)[i == 0]}, simplify = FALSE)

  # Matrix of response indicators
  ry <- !is.na(X)

  # Allocate a slot for the matrices storing which obs is in which pattern
  I <- sapply(rownames(R), function(x) NULL)

  # Populate the list
  for (s in 1:S) {
    # s <- 1
    index <- NULL
    for (i in 1:n) {
      # i <- 1
      if (all.equal(ry[i, ], Rl[s, ]) == TRUE) {
        index <- c(index, i)
      }
    }
    I[[s]] <- index
  }

  # Create the somi object
  somi <- list(
    S = S,
    O = O,
    M = M,
    I = I
  )

  # Assign class to object
  class(somi) <- c("somi", "list")

  # Output
  return(somi)

}