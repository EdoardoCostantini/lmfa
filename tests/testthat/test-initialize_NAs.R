# Project:   lmfa
# Objective: Test initialize_NAs function works as expected
# Author:    Edoardo Costantini
# Created:   2024-05-09
# Modified:  2024-05-09
# Notes: 

# Create objects for testing ---------------------------------------------------

# Set a seed 
set.seed(20240509)

# A numeric vector
x_numeric <- c(1, 5, 2, 4, NA, 2, 5, NA)

# A factor
x_factor <- factor(sample(c("A", "B", NA), 20, replace = TRUE))

# A data.frame with missing values
x_df <- mice::nhanes

# A matrix with missing values
x_mat <- as.matrix(mice::nhanes)

# Tests ------------------------------------------------------------------------

test_that("Output have the same attributes as input", {
    expect_equal(length(x_numeric), length(initialize_NAs(x_numeric)))
    expect_equal(length(x_factor), length(initialize_NAs(x_factor)))
    expect_equal(levels(x_factor), levels(initialize_NAs(x_factor)))
    expect_equal(dim(x_df), dim(apply(x_df, 2, initialize_NAs)))
    expect_equal(dim(x_mat), dim(apply(x_mat, 2, initialize_NAs)))
})