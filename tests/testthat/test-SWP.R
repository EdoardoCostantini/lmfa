# Project:   lmfa
# Objective: Test SWP function works as expected
# Author:    Edoardo Costantini
# Created:   2024-04-26
# Modified:  2024-04-26
# Notes: 

# Draw simple data
n <- 50
p <- 5
X <- data.frame(MASS::mvrnorm(n, rep(0, p), diag(p)))

# Make model matrix out of it
X_aug <- model.matrix(~., X)

# Make crossproduct matrix
T0 <- crossprod(as.matrix(X_aug))

# Create G-matrix as defined in Little Rubin p. 149
G <- matrix(NA, ncol = p + 1, nrow = p + 1)
G[, 1] <- colMeans(X_aug)
G[1, ] <- colMeans(X_aug)
G[-1, -1] <- crossprod(as.matrix(X)) / n
dimnames(G) <- dimnames(T0)

# Augmented covariance matrix from Little & Rubin p. 149 (eq 7.20)
theta <- matrix(NA, ncol = p + 1, nrow = p + 1)
theta[, 1] <- colMeans(X_aug)
theta[1, ] <- colMeans(X_aug)
theta[-1, -1] <- cov(X) * (n - 1) / n
theta[1, 1] <- -1
dimnames(theta) <- dimnames(T0)

# Define the dvs for a Multivariate regressions
dvs <- c("X1", "X5")

# Define the predictors
preds <- c("X3", "X4")

# Complicated but flexible way of writing the formula
formula_lm <- paste0("cbind(",
    paste0(dvs, collapse = ", "),
    ")",
    " ~ ",
    preds = paste0(preds, collapse = " + ")
)

# Fit the model with the alternatives
mlm0 <- lm(formula_lm, data = X)

# Store the coefficient matrix
coef_mat <- coef(mlm0)

# Store the name of the coefficients
coefs <- rownames(coef(mlm0))

# Sweep different matrices over coefs
sweep_T0 <- lmfa::SWP(T0, coefs)[coefs, dvs]
sweep_G <- lmfa::SWP(G, coefs)[coefs, dvs]
sweep_theta <- lmfa::SWP(theta, coefs[-1])[coefs, dvs]

# SWP works as expected
test_that("SWP works as expected", {
    expect_equal(coef_mat, sweep_T0)
    expect_equal(coef_mat, sweep_G)
    expect_equal(coef_mat, sweep_theta)
})
