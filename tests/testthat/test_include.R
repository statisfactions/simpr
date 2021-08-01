context("simpr::include")
library(dplyr)


blue_only = blueprint(x1 = ~ 2 + rnorm(n),
                 y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5))
spec = blue_only %>%
  meta(n = 100:101)

test_that("delayed fit() with produce_all() give same results as produce_sims()", {
  set.seed(100)
  lm_fit = blueprint(x1 = ~ 2 + rnorm(n),
                     y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    meta(n = 100:101) %>%
    produce_sims(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(100)
  lm_fit_include = blueprint(x1 = ~ 2 + rnorm(n),
                     y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    meta(n = 100:101) %>%
    fit(lm = ~lm(y ~ x1, data = .)) %>%
    produce_all(2)

  set.seed(100)
  lm_include_tricky = fit(lm =  ~lm(y ~ x1, data = .),
                          obj = blueprint(x1 = ~ 2 + rnorm(n),
                                y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    meta(n = 100:101)) %>%
    produce_all(2)

  expect_equivalent(lm_fit, lm_fit_include)
  expect_equivalent(lm_include_tricky, lm_fit_include)

})


test_that("delayed fit() with include() give same results as produce_sims()", {
  set.seed(100)
  lm_fit = spec %>%
    produce_sims(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(100)
  lm_fit_include = spec %>%
    fit(lm = ~lm(y ~ x1, data = .)) %>%
    produce_all(2)

  set.seed(100)
  lm_include_tricky = fit(lm =  ~lm(y ~ x1, data = .),
                          obj = spec %>%
                            include) %>%
    produce_all(2)

  expect_equivalent(lm_fit, lm_fit_include)
  expect_equivalent(lm_include_tricky, lm_fit_include)

})

test_that("delayed tidy() and glance() same as produce_sims()", {
 set.seed(101)

  lm_produce_fit = spec %>%
    produce_sims(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  lm_produce_tidy = tidy_fits(lm_produce_fit)
  lm_produce_glance = glance_fits(lm_produce_fit)

  lm_include_fit = spec %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(101)
  lm_include_tidy = produce_all(tidy_fits(lm_include_fit), 2)
  set.seed(101)
  lm_include_glance = produce_all(glance_fits(lm_include_fit), 2)

  expect_equivalent(lm_produce_tidy, lm_include_tidy)
  expect_equivalent(lm_produce_glance, lm_include_glance)
})








