context("simpr::generate")
library(tibble)


# Error options ------------
test_that(".quiet, .warn_on_error, .stop_on_error options work as expected", {
  buggy_spec = specify(y = ~ rnorm(n)) %>%
    define(n = c(-10, 10))

  message = "invalid arguments"


  expect_warning(generate(buggy_spec, 1), "Simulation produced errors")

  expect_silent(generate(buggy_spec, 1, .warn_on_error = FALSE))

  expect_message(generate(buggy_spec, 1, .warn_on_error = FALSE, .quiet = FALSE),
                 message)

  expect_error(generate(buggy_spec, 1, .stop_on_error = TRUE),
                 message)
})


# Global environment --------------
test_that("Metaparameters are not blocked by objects in calling environment", {

  n = "barf"

  ## This code should run without being confused by the `n` in the global environment
  expect_silent(out <- specify(x1 = ~ 2 + rnorm(n)) %>%
    define(n = 10) %>%
    generate(1))

})

test_that("Gen runs without warnings or messages", {
  expect_silent({
    out = specify(x1 = ~ 1,
                    x2 = ~ x1 + 1,
                    y = ~ x1 + x2) %>%
      generate(1)
  })

})




# Resimulating just a subset ----------
test_that("Subsetting in generate is equivalent to subsetting afterwards", {
  set.seed(100)
  sim_ref = specify(x1 = ~ 2 + rnorm(n)) %>%
    define(n = 10) %>%
    generate(10) %>%
    fit(dumb_model = ~ lm(x1 ~ 1))

  set.seed(100)
  sim_filt = specify(x1 = ~ 2 + rnorm(n)) %>%
    define(n = 10) %>%
    generate(10, .sim_id == 3) %>%
    fit(dumb_model = ~ lm(x1 ~ 1))

  set.seed(100)
  sim_filt_delay = specify(x1 = ~ 2 + rnorm(n)) %>%
    define(n = 10) %>%
    fit(dumb_model = ~ lm(x1 ~ 1)) %>%
    generate(10, .sim_id == 3)

  expect_equivalent(sim_ref[3,], sim_filt)
  expect_equivalent(sim_ref[3,], sim_filt_delay)
})

# Filtering works as expected on output from generate  -----

test_that("Filtering works as expected on output from generate()", {

  set.seed(500)
  gen_3 = specify(a = ~ runif(20)) %>%
    generate(3)

  row_2 = gen_3[2,]
  row_2_filter = gen_3 %>%
    filter(.sim_id == 2)

  expect_identical(row_2, row_2_filter)
})

