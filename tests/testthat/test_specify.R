context("simpr::specify")
library(tibble)

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


## Generate multiple variables from a single command -------------------

set.seed(100, kind = "L'Ecuyer-CMRG")
mat_1 = furrr::future_map(1, ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)),
                          .options = furrr::furrr_options(seed = TRUE))[[1]]

colnames(mat_1) = sprintf("x_%02.0f", 1:10)
mat_2 = mat_1
colnames(mat_2) = letters[1:10]

test_that("Autonumber when generating multiple columns with named argument", {
  set.seed(100, kind = "L'Ecuyer-CMRG")
  auto_out = specify(x = ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)), .sep = "_") %>%
    define(n = 10) %>%
    generate(1)

  expect_identical(auto_out$sim[[1]], as_tibble(mat_1))

})

test_that("Can refer to autonumbered columns in specify()", {
  comp_3 = as_tibble(mat_1) %>%
    dplyr::mutate(y = x_01 + x_02)

  set.seed(100)
  auto_refer = specify(x = ~ MASS::mvrnorm(30, rep(0, 10), Sigma = diag(10)),
                       y = ~ x_01 + x_02, .sep = "_") %>%
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

test_that("Can use names from DGP", {
  out1 = specify(x13 = ~ MASS::mvrnorm(10, mu = rep(0, 5), Sigma = diag(rep(1, 5))) %>%
                   data.frame %>%
                   setNames(letters[1:5])) %>%
    generate(1)

  expect_equal(names(out1$sim[[1]]), letters[1:5])


  out2 = specify(x13 = ~ MASS::mvrnorm(10, mu = rep(0, 5), Sigma = diag(rep(1, 5))) %>%
                   data.frame %>%
                   setNames(letters[1:5]),
                 .use_names = FALSE,
                 .sep = "...") %>%
    generate(1)

  expect_equal(names(out2$sim[[1]]), paste("x13", 1:5, sep = "..."))

})
