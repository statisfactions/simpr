context("simpr::define")
library(tibble)

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
