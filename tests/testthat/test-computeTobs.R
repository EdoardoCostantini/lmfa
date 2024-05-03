# Project:   lmfa
# Objective: Test computeTobs function works as expected
# Author:    Edoardo Costantini
# Created:   2024-04-26
# Modified:  2024-04-26
# Notes:

# Start with a dataset with no missing values
X <- mtcars

# Create SOMI object
SOMI <- createSOMI(X)

# Sample some weights
wt <- rep(1, nrow(X))

# Compute the Tobs
Tobs <- computeTobs(X = X, wt = wt, S = SOMI$S, I = SOMI$I)

# Number of missing data patterns
test_that("Correct elmenets with fully observed data", {
    expect_equal(nrow(X), Tobs[1, 1])
    expect_equal(colSums(x^2), diag(Tobs[-1, -1]))
    expect_equal(colSums(X), Tobs[1, -1])
    expect_equal(sum(X[, 1] * X[, 1]), Tobs[1 + 1, 1 + 1])
    expect_equal(sum(X[, 1] * X[, 2]), Tobs[1 + 1, 2 + 1])
})

# Now work with a dataset with missing values
X <- matrix(
    data = c(
        16, 20, 16, 20, -6, -4, 12, 24, 12, -6, 4, -8,
        8, 8, 26, -4, 4, 8, 20, 8, NA, NA, 20, -4,
        8, 4, -8, NA, 22, -8, 10, 20, 28, -20, -4, -4,
        4, 28, 24, 12, 8, 18, -8, 20, 24, -3, 8, -24,
        NA, 20, 24, 8, 12, NA
    ),
    ncol = 6,
    byrow = TRUE,
    dimnames = list(NULL, c("15P", "15L", "15H", "90P", "90L", "90H"))
)

# Create SOMI object
SOMI <- createSOMI(X)

# Sample some weights
wt <- rep(1, nrow(X))

# Compute the Tobs
Tobs <- computeTobs(X = X, wt = wt, S = SOMI$S, I = SOMI$I)

# Number of missing data patterns
test_that("Correct elmenets with fully observed data", {
    expect_equal(nrow(X), Tobs[1, 1])
    expect_equal(colSums(X, na.rm = TRUE), Tobs[1, -1])
})
