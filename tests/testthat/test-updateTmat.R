# Project:   lmfa
# Objective: Test updateTmat function works as expected
# Author:    Edoardo Costantini
# Created:   2024-04-26
# Modified:  2024-04-26
# Notes:

# Fake data
x <- as.data.frame(
    matrix(
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
)

# Create SOMI object
SOMI  = createSOMI(x)

# Sample some weights
wt    = runif(nrow(x))

# Compute starting matrices
Tmat  = computeTobs(X = x, wt = wt, S = SOMI$S, I = SOMI$I)
theta = augmentCov(covmat = cov(x, use = "complete.obs"),
                   center = colMeans(x, na.rm = TRUE)) # current augmented covariance matrix

# Update Tmat with fully observed missing data pattern
Tmat_update_1 <- updateTmat(
    X = x,
    wt = wt,
    Tmat = Tmat,
    theta = theta,
    obs = SOMI$I[[1]],
    v_mis = SOMI$M[[1]],
    v_obs = SOMI$O[[1]]
)

# Update Tmat with fully observed missing data pattern
Tmat_update_2 <- updateTmat(
    X = x,
    wt = wt,
    Tmat = Tmat,
    theta = theta,
    obs = SOMI$I[[4]],
    v_mis = SOMI$M[[4]],
    v_obs = SOMI$O[[4]]
)

# Number of missing data patterns
test_that("Correctly updates or doesnt", {
    expect_equal(Tmat, Tmat_update_1)
    expect_false(isTRUE(all.equal(Tmat, Tmat_update_2)))
})