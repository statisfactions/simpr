context("simpr::simpr_spec")
library(dplyr)

test_that("is.simpr_spec works on output of both specify() and define", {
  specify(a = ~ rnorm(10)) %>%
    is.simpr_spec() %>%
    expect_true()

  specify(a = ~ rnorm(n)) %>%
    define(n = 20) %>%
    is.simpr_spec() %>%
    expect_true()

})
