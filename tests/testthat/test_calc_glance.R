context("calc_glance")
library(dplyr)

test_that("calc_glance correctly returns broom::glance output", {
  ## Reference
  set.seed(100)
  x1 = 2 + rnorm(10)
  x2 = x1 + rnorm(10)

  broom_target = broom::glance(lm(x2 ~ x1))

  ## calc_glance
  set.seed(100)
  lin_test = variables(y1 = ~ 2 + rnorm(10),
            y2 = ~ y1 + rnorm(10)) %>%
    gen(1) %>%
    fit(linear = ~ lm(y2 ~ y1, data = .)) %>%
    calc_glance

  lin_test %>%
    select(-rep, -Source) %>%
    as_tibble %>%
    expect_identical(broom_target)

})

