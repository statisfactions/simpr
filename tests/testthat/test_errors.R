context("simpr::errors")
library(dplyr)

test_that("errors produce new column", {

  errgt = function(x) {stopifnot(x < 3); x}
set.seed(100)
blue_only = specify(x1 = ~ 2 + rnorm(n),
                      y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5) + errgt(x1))
spec = blue_only %>%
  define(n = 10)

err_out = generate(spec, 30, .warn_on_error = FALSE, .options = furrr_options(seed = TRUE,
                                                                                 globals = list(errgt = errgt)))

expect_true(".sim_error" %in% names(err_out))

expect_true(all(grepl("x < 3 are not all TRUE", na.omit(err_out$.sim_error))))

})
