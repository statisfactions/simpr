context("simpr::simpr_spec")
library(dplyr)

test_that("order of specify and meta are equivalent with variable named 'x'", {
  bp_meta = specify(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
    define(n = c(10, 20, 30),
         S = list(independent = diag(2), correlated = diag(2) + 2))

  meta_bp = define(n = c(10, 20, 30),
                 S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
    specify(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S))

  expect_equivalent(bp_meta, meta_bp)
})

test_that("order of specify and meta are equivalent with NO variable named 'x'", {
  bp_meta = specify(y = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
    define(n = c(10, 20, 30),
           S = list(independent = diag(2), correlated = diag(2) + 2))

  meta_bp = define(n = c(10, 20, 30),
                   S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
    specify(y = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S))

  expect_equivalent(bp_meta, meta_bp)
})
