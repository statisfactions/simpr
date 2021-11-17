context("simpr::gen")

## Metaparameters and the global environment --------------

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

test_that("Earlier variables have access to output of later variables", {
 out = specify(x1 = ~ 1,
                 x2 = ~ x1 + 1,
                 y = ~ x1 + x2) %>%
   generate(1)

 ref <-
   list(structure(list(x1 = 1, x2 = 2, y = 3), class = c("tbl_df",
                                                         "tbl", "data.frame"), row.names = c(NA, -1L)))

 expect_identical(out$sim, ref)
})

test_that("Gen runs without warnings or messages", {
  expect_silent({
    out = specify(x1 = ~ 1,
                    x2 = ~ x1 + 1,
                    y = ~ x1 + x2) %>%
      generate(1)
  })

})

## define can handle lists which can contain multiple matrices, etc.

test_that("define() can handle lists", {

  set.seed(100, kind = "L'Ecuyer-CMRG")
  possible_S = list(independent = diag(2), correlated = diag(2) + 2)
  possible_n = 10
  mat_refs = furrr::future_map(possible_S, ~ MASS::mvrnorm(possible_n, rep(0, 2), Sigma = .),
                              .options = furrr::furrr_options(seed = TRUE))

  set.seed(100, kind = "L'Ecuyer-CMRG")
  meta_list_out <- specify(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
    define(n = 10,
         S = possible_S) %>%
    generate(1)

  expect_equivalent(mat_refs, purrr::map(meta_list_out$sim, as.matrix))

})



## Generate multiple variables from a single command -------------------

set.seed(100, kind = "L'Ecuyer-CMRG")
mat_1 = furrr::future_map(1, ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)),
                                           .options = furrr::furrr_options(seed = TRUE))[[1]]

colnames(mat_1) = sprintf("x_%02.0f", 1:10)
mat_2 = mat_1
colnames(mat_2) = letters[1:10]

test_that("Autonumber when generating multiple columns with named argument", {
  set.seed(100, kind = "L'Ecuyer-CMRG")
  auto_out = specify(x = ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)), sep = "_") %>%
    define(n = 10) %>%
    generate(1)

  expect_identical(auto_out$sim[[1]], as_tibble(mat_1))

})

test_that("Can refer to autonumbered columns in specify()", {
  comp_3 = as_tibble(mat_1) %>%
    dplyr::mutate(y = x_01 + x_02)

  set.seed(100)
  auto_refer = specify(x = ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)), sep = "_",
                         y = ~ x_01 + x_02) %>%
    define(n = 10) %>%
    generate(1)

  expect_identical(auto_refer$sim[[1]], comp_3)
})

test_that("Multiple columns with two-sided formulas and unnamed arguments", {
  set.seed(100)
  cbind_out = specify(cbind(a, b, c, d, e, f, g, h, i, j) ~
                          MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10))) %>%
    generate(1)

  expect_identical(cbind_out$sim[[1]], as_tibble(mat_2))
})

test_that("Can refer to two-sided formula columns as arguments in specify()", {
  comp_4 = as_tibble(mat_2) %>%
    dplyr::mutate (y = a + b)

  set.seed(100)
  cbind_refer = specify(cbind(a, b, c, d, e, f, g, h, i, j) ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)),
                          y = ~ a + b) %>%
    generate(1)

  expect_identical(cbind_refer$sim[[1]], comp_4)
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



