# Project:   lmfa
# Objective: Test RSWP function works as expected
# Author:    Edoardo Costantini
# Created:   2024-04-26
# Modified:  2024-04-26
# Notes:

# Set a seed for replicatibility
set.seed(100)

# Generate symmetric positive definite matrix
Sigma_OG <- rWishart(1, 4, diag(3))[, , 1]

# Sweep all the columns to produce the inverse, then reverse sweep
Sigma_RWSP <- RSWP(SWP(Sigma_OG, 1:3), 1:3)

# RSWP works as expected
test_that("RSWP works as expected", {
    expect_equal(Sigma_OG, Sigma_RWSP)
})
