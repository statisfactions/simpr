context("simpr::as_sims")

test_that("add_sims works with mutate", {
  set.seed(203)
  lin_test = blueprint(y1 = ~ 2 + rnorm(10),
                       y2 = ~ y1 + rnorm(10)) %>%
    generate(1) %>%
    as_sims %>% mutate(y3 = y1 + y2) %>%
    as_tibble

  set.seed(203)
  lin_test2 = blueprint(y1 = ~ 2 + rnorm(10),
                       y2 = ~ y1 + rnorm(10),
                       y3 = ~ y1 + y2) %>%
    generate(1)

  expect_equivalent(lin_test, lin_test2)

})

test_that("reshaping works as expected", {
  rt_spec = blueprint(
    # ID numbers for each participant
    id = ~ seq_len(n),
    # control condition RT
    ctrl = ~ rlnorm(n, # sample size
                    meanlog = mu, # difficulty (mean)
                    sdlog = sigma), # scale (sd)
    # experimental condition RT
    expl = ~ rlnorm(n, # sample size
                    meanlog = mu + mu_diff, # difficulty (mean)
                    sdlog = sigma + sig_diff) # scale (sd)
  ) %>%
    # define the meta-parameters
    meta(n = 100, # number of trials
         mu = c(-.5), # note: exp(mu) is the median RT, so these correspond to .6 and 1 sec, respectively
         sigma = c(.2), # standard deviation (scale) of the log-normal -- corresp. to 1.22 & 1.65 secs
         mu_diff = c(.2), # how much mu shifts from Ctrl to Exp -- .2 is a 22% increase, .4 is 49% incr
         sig_diff = c(.2) # how much sigma shifts from Ctrl to Exp (log % increase in SD)
    )

  set.seed(101)
  rt_gen = rt_spec %>%
    generate(1) %>%
    as_sims() %>%
    pivot_longer(-id, names_to = "Condition", values_to = "RT")

  expect_equal(names(rt_gen$sim[[1]]), c("id", "Condition", "RT"))

  ## Delayed
  set.seed(101)
  rt_gen_delayed = rt_spec %>%
    as_sims() %>%
    pivot_longer(-id, names_to = "Condition", values_to = "RT") %>%
    generate(1)

  expect_equivalent(rt_gen, rt_gen_delayed)

})
