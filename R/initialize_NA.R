#' Initialize missing values
#'
#' Initialize missing values on a data column. If there are no missing values, the original vector is returned.
#'
#' @param x A vector with missing values
#' @param replace A boolean unit vector describing whether sampling should be done with replacement: yes (TRUE), or no (FALSE)
#' @return A vector with missing values replaced by sampled observed values
#'
#' @author Edoardo Costantini, 2023
#' @examples
#' # Create a data matrix
#' x <- c(1, 5, 2, 4, NA, 2, 5, NA)
#'
#' # Use function
#' initialize_NAs(x)
#'
#' @export
initialize_NAs <- function(x, replace = TRUE) {
    # Create a boolean response vector
    wy <- is.na(x)

    # If there are missing values
    if (any(wy)) {
        # Create vector of observed values
        obs <- na.omit(x)

        # Count the number of missing values
        n_NAs <- sum(is.na(x))

        # Replace missing values with a random draw
        x[wy] <- sample(x = obs, size = n_NAs, replace = replace)
    }

    # Return the original or modified vector
    return(x)
}
