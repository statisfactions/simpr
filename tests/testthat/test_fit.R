context("simpr::fit")

test_that("Multiple fit functions give different results", {
  set.seed(100)
  chisq_spec = blueprint(x1 = ~rnorm(n),
                         x2 = ~x1 + rnorm(n, 0, sd = 2),
                         c1 = ~ cut(x1, breaks = b) %>% as.numeric,
                         c2 = ~ cut(x2, breaks = b) %>% as.numeric) %>%
    meta(n = c(50, 100),
         b = c(2, 10))

  chisq_fit = chisq_spec %>%
    produce_sims(5) %>%
    fit(ChiSq = ~ suppressWarnings(chisq.test(.$c1, .$c2)),
        Unknown_Continuous_Correlation = ~cor.test(.$x1, .$x2))

  ## These should NOT be the same!
  expect_false(identical(chisq_fit$ChiSq, chisq_fit$Unknown_Continuous_Correlation))

})
