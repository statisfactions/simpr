context("simpr::generate")
library(tibble)

## Global environment --------------
test_that("Metaparameters are not blocked by objects in calling environment", {

  n = "barf"

  ## This code should run without being confused by the `n` in the global environment
  expect_silent(out <- specify(x1 = ~ 2 + rnorm(n)) %>%
    define(n = 10) %>%
    generate(1))

})


test_that("generate() doesn't put objects in global environment", {

  if(exists("xyz123", envir = .GlobalEnv))
    rm("xyz123", envir = .GlobalEnv)

  out = specify(xyz123 = ~ 2 + rnorm(n)) %>%
    define(n = 10) %>%
    generate(1)

  expect_false(exists("xyz123", envir = .GlobalEnv))

})

test_that("Gen runs without warnings or messages", {
  expect_silent({
    out = specify(x1 = ~ 1,
                    x2 = ~ x1 + 1,
                    y = ~ x1 + x2) %>%
      generate(1)
  })

})




## Resimulating just a subset ----------
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



