# Project:   lmfa
# Objective: Test createSOMI function works as expected
# Author:    Edoardo Costantini
# Created:   2024-04-26
# Modified:  2024-04-26
# Notes:

set.seed(100)

# Define an input matrix with simple known missing data
data_matrix <- matrix(
    c(
        0, 1, NA,
        1, 2, NA,
        NA, 2, 3,
        5, NA, NA
    ),
    ncol = 3,
    dimnames = list(NULL, paste0("x", 1:3)),
    byrow = TRUE
)

# Use the function
SOMI_object <- createSOMI(X = data_matrix)

# Number of missing data patterns
test_that("Number of missing data patterns", {
    expect_equal(SOMI_object$S, 3)
    expect_equal(length(SOMI_object$O), SOMI_object$S)
    expect_equal(length(SOMI_object$M), SOMI_object$S)
    expect_equal(length(SOMI_object$I), SOMI_object$S)
})

# Number of missing data patterns
test_that("Unique memberships to missing data patterns", {
    expect_false(SOMI_object$M$S1 %in% SOMI_object$O$S1)
    expect_false(SOMI_object$O$S2 %in% SOMI_object$M$S2)
    expect_false(SOMI_object$M$S3 %in% SOMI_object$O$S3)
    expect_equal(
        unlist(SOMI_object$I, use.names = FALSE),
        unique(unlist(SOMI_object$I))
    )
})
