context("simpr::tidy_fits")
library(tibble)

test_that("Calc tidy terms match terms from fit",
          {
            set.seed(100)
            lm_fit = specify(x1 = ~ 2 + rnorm(n),
                               y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
              define(n = 100:101) %>%
              generate(2) %>%
              fit(lm = ~lm(y ~ x1, data = .))

            lm_tidy = lm_fit %>%
              tidy_fits

            lm_tidy_unique_terms = lm_tidy %>%
              dplyr::count(n, rep, term, name = "count")
            expect_true(all(lm_tidy_unique_terms$count == 1))

            lm_fit_coef = purrr::map_df(lm_fit$lm, ~ coef(.) %>% t %>%
                  as.data.frame(check.names = F)) %>%
              dplyr::bind_cols(lm_fit %>% tibble::as_tibble() %>% select(.sim_id, n, rep), .) %>%
              dplyr::arrange(n, rep)

            lm_tidy_coef = lm_tidy %>%
              dplyr::select(.sim_id, n, rep, term, estimate) %>%
              tidyr::spread(term, estimate) %>%
              dplyr::arrange(n, rep)

            expect_equivalent(lm_fit_coef, lm_tidy_coef)
          })


test_that("Each iteration of simulation has model terms listed correctly in tidy_fits output", {
  # define metaparamters (to use in define() and test)
  meta_list = list(n = seq(100, 300, by = 20),
  b1 = 1,
  b2 = 1,
  g1 = seq(-1, 1, by = 0.5),
  rep = 5)

  # create a dataframe of all possible combinations of parameters (to check)
  all_combos <- expand.grid(meta_list)

  # run the simulation
  set.seed(100)
  simpr_spec = specify(x1 = ~ 2 + rnorm(n),
                         x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
                         y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = 3)) %>%
    define(n = meta_list$n,
         b1 = meta_list$b1,
         b2 = meta_list$b2,
         g1 = meta_list$g1)

  simpr_gen = simpr_spec %>%
    generate(meta_list$rep) %>%
    fit(lm = ~lm(y ~ x1*x2, data = .))
  simpr_calc = simpr_gen %>%
    tidy_fits

  ## Count how many times a given combo of metaparameters and rep occur;
  ## we expect ONLY ONE each time
  tidy_unique_terms = simpr_calc %>%
    dplyr::group_by_at(.vars = c(names(meta_list), "term")) %>%
    dplyr::tally(name = "count")
  expect_true(all(tidy_unique_terms$count == 1))

})

## Errors show up -----
test_that("Errors show up in tidied output.", {

  buggy_fit = specify(y = ~ rnorm(size)) %>%
    define(size = c(-10, 10)) %>%
    generate(1, .warn_on_error = FALSE) %>%
    fit(t_test = ~ t.test(y),
        chisq = ~ chisq.test(y),
        .warn_on_error = FALSE) %>%
    tidy_fits()

  expect_match(buggy_fit$.sim_error[[1]], "invalid arguments")
  expect_match(buggy_fit$.sim_error[[2]], "invalid arguments")
  expect_equal(buggy_fit$.sim_error[[3]], NA_character_)
  expect_equal(buggy_fit$.sim_error[[4]], NA_character_)

  expect_equal(buggy_fit$.fit_error,
               c("Error in t.test(y): object 'y' not found\n",
                 "Error in is.data.frame(x): object 'y' not found\n",
                 NA,
                 "Error in chisq.test(y): all entries of 'x' must be nonnegative and finite\n"
               ))

})

