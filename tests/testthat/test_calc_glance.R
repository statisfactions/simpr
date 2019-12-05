context("calc_glance")

test_that("calc_glance correctly returns broom::glance output", {
  ## Reference
  set.seed(100)
  x1 = 2 + rnorm(10)
  x2 = x1 + rnorm(10)

  broom_target = broom::glance(lm(x2 ~ x1))

  ## calc_glance
  set.seed(100)
  lin_test = variables(y1 = ~ 2 + rnorm(10),
            y2 = ~ y1 + rnorm(10)) %>%
    gen(1) %>%
    fit(linear = ~ lm(y2 ~ y1, data = .))

  expect_identical(broom_target, lin_test$linear[[1]])

})

