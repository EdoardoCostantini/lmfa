# Project:   lmfa
# Objective: Test whether the observed_LL function is behaving as expected
# Author:    Edoardo Costantini
# Created:   2024-05-09
# Modified:  2024-05-09
# Notes: 

# Set a seed
set.seed(1000)

# Obtain a subset of original ESM
x <- ESM[sample(1:nrow(ESM), 1e3), c(
    "Interested",
    "Joyful",
    "Nervous",
    "Listless"
)]

# Initialize parameters for step 1
parms <- initializeStep1(
    x = x, 
    n_sub = nrow(x), 
    n_state = 3, 
    n_fact = c(3, 3, 3),
    J = ncol(x),
    startval = "mclust",
    RandVec = RandVec,
    ini_mclust = Mclust(x, G = 3, verbose = FALSE)$classification,
    ini_mclust_specific = ini_mclust_specific
)

# Compute multivariate densities
saveDMV <- DMV(
    x,
    Lambda_k = parms$Lambda_k,
    Psi_k = parms$Psi_k,
    n_state = 3,
    J = ncol(x),
    n_sub = nrow(x),
    nu_k = parms$nu_k
)

# Use function
LL <- observed_LL(
    dmv = saveDMV,
    n = nrow(x),
    n_state = 3,
    pi_k = parms$pi_k
)

# Test the function returns a numeric vector of length 1
testthat::expect_true(is.numeric(LL))
testthat::expect_true(length(LL) == 1)