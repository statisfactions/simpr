context("simpr::gen")
library(tidyverse)

## Metaparameters and the global environment --------------

test_that("Metaparameters are not blocked by objects in calling environment", {

  n = "barf"

  ## This code should run without being confused by the `n` in the global environment
  out = variables(x1 = ~ 2 + rnorm(n)) %>%
    meta(n = 10) %>%
    gen(1)

})

test_that("gen() doesn't put objects in global environment", {

  if(exists("xyz123", envir = .GlobalEnv))
    rm("xyz123", envir = .GlobalEnv)

  out = variables(xyz123 = ~ 2 + rnorm(n)) %>%
    meta(n = 10) %>%
    gen(1)

  expect_false(exists("xyz123", envir = .GlobalEnv))

})

test_that("Earlier variables have access to output of later variables", {
 out = variables(x1 = ~ 1,
                 x2 = ~ x1 + 1,
                 y = ~ x1 + x2) %>%
   gen(1)

 ref <-
   list(structure(list(x1 = 1, x2 = 2, y = 3), class = c("tbl_df",
                                                         "tbl", "data.frame"), row.names = c(NA, -1L)))

 expect_identical(out$sim_cell, ref)
})

test_that("Gen runs without warnings or messages", {
  expect_silent({
    out = variables(x1 = ~ 1,
                    x2 = ~ x1 + 1,
                    y = ~ x1 + x2) %>%
      gen(1)
  })

})

## Generate multiple variables from a single command

library(MASS)
set.seed(100)

mat_1 = mvrnorm(30, rep(0, 10), Sigma = diag(10))
mat_2 = mat_1


colnames(mat_1) = sprintf("x_%02.0f", 1:10)
colnames(mat_2) = letters[1:10]

test_that("Autonumber when generating multiple columns with named argument", {
  set.seed(100)
  auto_out = variables(x = ~ mvrnorm(30, rep(0, 10), Sigma = diag(10)), sep = "_") %>%
    gen(1)

  expect_identical(out$sim_cell[[1]], as_tibble(mat_1))

})

test_that("Multiple columns with two-sided formulas and unnamed arguments", {
  set.seed(100)
  cbind_out = variables(cbind(a, b, c, d, e, f, g, h, i, j) ~ mvrnorm(30, rep(0, 10), Sigma = diag(10))) %>%
    gen(1)

  expect_identical(out$sim_cell[[1]], as_tibble(mat_2))
})



