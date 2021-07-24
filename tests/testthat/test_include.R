context("simpr::include")
library(dplyr)


blue_only = blueprint(x1 = ~ 2 + rnorm(n),
                 y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5))
spec = blue_only %>%
  meta(n = 100:101)

test_that("include gives errors after produce has been run", {

  expect_error(spec %>%
    produce(2) %>%
    include, regexp = "has already been run")

  expect_error(spec %>%
                 produce(2) %>%
                 fit(lm = ~lm(y ~ x1, data = .)) %>%
                 include, regexp = "has already been run")

  expect_error(spec %>%
                 produce(2) %>%
                 fit(lm = ~lm(y ~ x1, data = .)) %>%
                 tidy_fits %>%
                 include, regexp = "has already been run")

  expect_error(spec %>%
                 produce(2) %>%
                 fit(lm = ~lm(y ~ x1, data = .)) %>%
                 glance_fits %>% include, regexp = "has already been run")
})

test_that("include accurately captures info from spec objects", {
  include_blue = include(blue_only)
  include_spec = include(spec)

  expect_equivalent(blue_only, include_blue)
  expect_equivalent(spec, include_spec)

})

test_that("delayed evaluation with include() give same results as produce()", {
  set.seed(100)
  lm_fit = blueprint(x1 = ~ 2 + rnorm(n),
                     y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    meta(n = 100:101) %>%
    produce(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(100)
  lm_fit_include = blueprint(x1 = ~ 2 + rnorm(n),
                     y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    meta(n = 100:101) %>%
    include %>%
    fit(lm = ~lm(y ~ x1, data = .)) %>%
    produce(2)

  set.seed(100)
  lm_include_tricky = fit(lm =  ~lm(y ~ x1, data = .),
                          obj = blueprint(x1 = ~ 2 + rnorm(n),
                                y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    meta(n = 100:101) %>%
    include) %>%
    produce(2)

  expect_equivalent(lm_fit, lm_fit_include)
  expect_equivalent(lm_include_tricky, lm_fit_include)

})



