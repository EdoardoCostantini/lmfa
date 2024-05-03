# Project:   lmfa
# Objective: Test augmentCov function works as expected
# Author:    Edoardo Costantini
# Created:   2024-04-26
# Modified:  2024-04-26
# Notes:

# Run function
aug_cov <- augmentCov(
    covmat = cov(mtcars),     # covariance matrix of a dataset
    center = colMeans(mtcars) # vector of means of a dataset
)

# Correct output
test_that("Correct elmenets with fully observed data", {
    expect_equal(-1, aug_cov[1, 1])
    expect_equal(colMeans(mtcars), aug_cov[-1, 1])
    expect_equal(cov(mtcars), aug_cov[-1, -1])
})
