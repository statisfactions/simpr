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

  expect_identical(attributes(blue_only)[setdiff(names(attributes(blue_only)),
                                                 "class")],
                   attributes(include_blue)[setdiff(names(attributes(blue_only)),
                                                          "class")])
  expect_equal(class(blue_only), attr(include_blue, "include_class"))

  expect_identical(attributes(include_spec)[setdiff(names(attributes(include_spec)),
                                                 "class")],
                   attributes(include_spec)[setdiff(names(attributes(include_spec)),
                                                    "class")])

  expect_equal(class(spec), attr(include_spec, "include_class"))

})



