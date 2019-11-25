context("simpr::gen metaparameter tibble")
library(tidyverse)

test_that("Calc tidy terms match terms from fit",
          {
            set.seed(100)
            lm_fit = variables(x1 = ~ 2 + rnorm(n),
                               y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
              meta(n = 100:101) %>%
              gen(2) %>%
              fit(lm = ~lm(y ~ x1, data = .))

            lm_tidy = lm_fit %>%
              calc_tidy

            lm_tidy_unique_terms = lm_tidy %>%
              count(n, rep, term, name = "count")
            expect_equal(lm_tidy %>% select(n, rep, term) %>%
                         anyDuplicated, 0)

            lm_fit_coef = map_df(lm_fit$lm, ~ coef(.) %>% t %>%
                  as.data.frame(check.names = F)) %>%
              bind_cols(lm_fit %>% select(n, rep), .) %>%
              arrange(n, rep)

            lm_tidy_coef = lm_tidy %>%
              select(n, rep, term, estimate) %>%
              spread(term, estimate) %>%
              arrange(n, rep)

            expect_equal(lm_fit_coef, lm_tidy_coef)
          })


test_that("Each iteration of simulation has model terms listed correctly", {
  # define metaparamters (to use in meta() and test)
  meta_list = list(n = seq(100, 300, by = 20),
  b1 = 1,
  b2 = 1,
  g1 = seq(-1, 1, by = 0.5),
  reps = 5)

  # create a dataframe of all possible combinations of parameters (to check)
  all_combos <- expand.grid(meta_list)

  # run the simulation
  set.seed(100)
  simpr_spec = variables(x1 = ~ 2 + rnorm(n),
                         x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
                         y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = 3)) %>%
    meta(n = meta_list$n,
         b1 = meta_list$b1,
         b2 = meta_list$b2,
         g1 = meta_list$g1)

  simpr_gen = simpr_spec %>%
    gen(meta_list$reps) %>%
    fit(lm = ~lm(y ~ x1*x2, data = .))
  simpr_calc = simpr_gen %>%
    calc_tidy

  # organize by the metaparameters (not necessary anymore)
  simpr_calc2 <- simpr_calc %>%
    dplyr::arrange(n, b1, b2, g1, rep)

  # loop through all combos and check
  for (r in 1:nrow(all_combos)) { # for each row in metaparameter combo df
    cur_params <- all_combos[r,]
    cur_sub <- simpr_calc2 %>%
      dplyr::filter(n==cur_params$n, b1==cur_params$b1, b2==cur_params$b2,
                    g1==cur_params$g1, rep==cur_params$rep)

    # we expect ALL TERMS in the model to have unique labels
    expect_true(
      cur_sub %>% # is the number of total term labels...
        dplyr::select(term) %>%
        nrow() == cur_sub %>% # equal to...
          dplyr::select(term) %>%
          unique() %>% # the number of UNIQUE term labels?
          nrow()
    )

    # # we do NOT expect these terms to have equal values
    # expect_false(expect_equal(
    #   simpr_calc2 %>%
    #     dplyr::filter(n==n[1], g1==g1[1], s==s[1], rep==1) %>%
    #     dplyr::select(estimate)
    # ))

  }
})
