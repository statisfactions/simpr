context("simpr::simpr_spec")
library(dplyr)

test_that("order of blueprint and meta are equivalent", {
  bp_meta = blueprint(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
    meta(n = c(10, 20, 30),
         S = list(independent = diag(2), correlated = diag(2) + 2))

  meta_bp = meta(n = c(10, 20, 30),
                 S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
    blueprint(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S))

  expect_equivalent(bp_meta, meta_bp)
})
