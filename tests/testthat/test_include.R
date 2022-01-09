context("simpr::include")
library(dplyr)


blue_only = specify(x1 = ~ 2 + rnorm(n),
                 y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5))
spec = blue_only %>%
  define(n = 100:101)

test_that("delayed fit() with generate() give same results as generate()", {
  set.seed(100)
  lm_fit = specify(x1 = ~ 2 + rnorm(n),
                     y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    define(n = 100:101) %>%
    generate(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(100)
  lm_fit_include = specify(x1 = ~ 2 + rnorm(n),
                     y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    define(n = 100:101) %>%
    fit(lm = ~lm(y ~ x1, data = .)) %>%
    generate(2)

  set.seed(100)
  lm_include_tricky = fit(lm =  ~lm(y ~ x1, data = .),
                          obj = specify(x1 = ~ 2 + rnorm(n),
                                y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
    define(n = 100:101)) %>%
    generate(2)

  expect_equivalent(lm_fit, lm_fit_include)
  expect_equivalent(lm_include_tricky, lm_fit_include)

})


test_that("delayed fit() with include() give same results as generate()", {
  set.seed(100)
  lm_fit = spec %>%
    generate(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(100)
  lm_fit_include = spec %>%
    fit(lm = ~lm(y ~ x1, data = .)) %>%
    generate(2)

  set.seed(100)
  lm_include_tricky = fit(lm =  ~lm(y ~ x1, data = .),
                          obj = spec) %>%
    generate(2)

  expect_equivalent(lm_fit, lm_fit_include)
  expect_equivalent(lm_include_tricky, lm_fit_include)

})

test_that("delayed tidy() and glance() same as generate()", {
 set.seed(101)

  lm_produce_fit = spec %>%
    generate(2) %>%
    fit(lm = ~lm(y ~ x1, data = .))

  lm_produce_tidy = tidy_fits(lm_produce_fit)
  lm_produce_glance = glance_fits(lm_produce_fit)

  lm_include_fit = spec %>%
    fit(lm = ~lm(y ~ x1, data = .))

  set.seed(101)
  lm_include_tidy = generate(tidy_fits(lm_include_fit), 2)
  set.seed(101)
  lm_include_glance = generate(glance_fits(lm_include_fit), 2)

  expect_equivalent(lm_produce_tidy, lm_include_tidy)
  expect_equivalent(lm_produce_glance, lm_include_glance)

  expect_identical(class(lm_produce_tidy), class(lm_include_tidy))
  expect_identical(class(lm_produce_glance), class(lm_include_glance))
})










