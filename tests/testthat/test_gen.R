context("simpr::gen")
library(MASS)
library(dplyr)
select = dplyr::select

## Metaparameters and the global environment --------------

test_that("Metaparameters are not blocked by objects in calling environment", {

  n = "barf"

  ## This code should run without being confused by the `n` in the global environment
  expect_silent(out <- blueprint(x1 = ~ 2 + rnorm(n)) %>%
    meta(n = 10) %>%
    produce(1))

})

test_that("produce() doesn't put objects in global environment", {

  if(exists("xyz123", envir = .GlobalEnv))
    rm("xyz123", envir = .GlobalEnv)

  out = blueprint(xyz123 = ~ 2 + rnorm(n)) %>%
    meta(n = 10) %>%
    produce(1)

  expect_false(exists("xyz123", envir = .GlobalEnv))

})

test_that("Earlier variables have access to output of later variables", {
 out = blueprint(x1 = ~ 1,
                 x2 = ~ x1 + 1,
                 y = ~ x1 + x2) %>%
   produce(1)

 ref <-
   list(structure(list(x1 = 1, x2 = 2, y = 3), class = c("tbl_df",
                                                         "tbl", "data.frame"), row.names = c(NA, -1L)))

 expect_identical(out$sim_cell, ref)
})

test_that("Gen runs without warnings or messages", {
  expect_silent({
    out = blueprint(x1 = ~ 1,
                    x2 = ~ x1 + 1,
                    y = ~ x1 + x2) %>%
      produce(1)
  })

})

## meta can handle lists which can contain multiple matrices, etc.

test_that("meta() can handle lists", {
  expect_silent(meta_list_out <- blueprint(x = ~ mvrnorm(n, rep(0, 2), Sigma = S)[, 2, drop = TRUE],
            y = ~ x + rnorm(n)) %>%
    meta(n = c(10, 20, 30),
         S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
    produce(1))

})



## Generate multiple variables from a single command -------------------

set.seed(100)

mat_1 = mvrnorm(30, rep(0, 10), Sigma = diag(10))
mat_2 = mat_1


colnames(mat_1) = sprintf("x_%02.0f", 1:10)
colnames(mat_2) = letters[1:10]

test_that("Autonumber when generating multiple columns with named argument", {
  set.seed(100)
  auto_out = blueprint(x = ~ mvrnorm(30, rep(0, 10), Sigma = diag(10)), sep = "_") %>%
    meta(n = 10) %>%
    produce(1)

  expect_identical(auto_out$sim_cell[[1]], as_tibble(mat_1))

})

test_that("Can refer to autonumbered columns in blueprint()", {
  comp_3 = as_tibble(mat_1) %>%
    mutate (y = x_01 + x_02)

  set.seed(100)
  auto_refer = blueprint(x = ~ mvrnorm(30, rep(0, 10), Sigma = diag(10)), sep = "_",
                         y = ~ x_01 + x_02) %>%
    meta(n = 10) %>%
    produce(1)

  expect_identical(auto_refer$sim_cell[[1]], comp_3)
})

test_that("Multiple columns with two-sided formulas and unnamed arguments", {
  set.seed(100)
  cbind_out = blueprint(cbind(a, b, c, d, e, f, g, h, i, j) ~ mvrnorm(30, rep(0, 10), Sigma = diag(10))) %>%
    produce(1)

  expect_identical(cbind_out$sim_cell[[1]], as_tibble(mat_2))
})

test_that("Can refer to two-sided formula columns as arguments in blueprint()", {
  comp_4 = as_tibble(mat_2) %>%
    mutate (y = a + b)

  set.seed(100)
  cbind_refer = blueprint(cbind(a, b, c, d, e, f, g, h, i, j) ~ mvrnorm(30, rep(0, 10), Sigma = diag(10)),
                          y = ~ a + b) %>%
    produce(1)

  expect_identical(cbind_refer$sim_cell[[1]], comp_4)
})



