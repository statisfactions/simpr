context("glance_fits")
library(tibble)

test_that("glance_fits correctly returns broom::glance output", {
  ## Reference
  set.seed(100, kind = "L'Ecuyer-CMRG")
  broom_target = furrr::future_map(1, function(x) {
    x1 = 2 + rnorm(10)
    x2 = x1 + rnorm(10)

    broom::glance(lm(x2 ~ x1))
  }, .options = furrr_options(seed = TRUE))[[1]] %>%
    mutate(.sim_id = 1) %>%
    relocate(.sim_id)

  ## glance_fits
  set.seed(100, kind = "L'Ecuyer-CMRG")
  lin_test = specify(y1 = ~ 2 + rnorm(10),
            y2 = ~ y1 + rnorm(10)) %>%
    generate(1, .options = furrr_options(seed = TRUE)) %>%
    fit(linear = ~ lm(y2 ~ y1, data = .)) %>%
    glance_fits

  broom_test =  lin_test %>%
    dplyr::select(-rep, -Source) %>%
    as_tibble

    expect_equivalent(broom_test, broom_target)

})

