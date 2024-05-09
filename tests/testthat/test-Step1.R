# Project:   lmfa
# Objective: Test Step1 function works as expected
# Author:    Edoardo Costantini
# Created:   2024-05-09
# Modified:  2024-05-09
# Notes:

# Set a seed
set.seed(1000)

# Obtain a subset of original ESM
ESM_subset <- ESM[sample(1:nrow(ESM), 1e3), c(
  "Interested",
  "Joyful",
  "Nervous",
  "Listless"
)]

# Run step1
step1_res <- step1(
  data = ESM_subset,
  indicators = c(
    "Interested",
    "Joyful",
    "Nervous",
    "Listless"
  ),
  n_state = 3,
  n_fact = c(3, 3, 3),
  modelselection = FALSE,
  n_starts = 2,
  n_initial_ite = 2,
  n_m_step = 5,
  em_tolerance = 1e-8,
  m_step_tolerance = 1e-3,
  max_iterations = 5,
  n_mclust = 2
)

# Test the function is returning the right class
testthat::expect_true(class(step1_res) == "lmfa_step1")
