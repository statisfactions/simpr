library(simpr)
library(tidyverse)
library(lavaan)
library(Matrix)

mot_ach = .5
eng_ach = c(.3, .5, .7)
eng_mot = c(.3, .5, .7)


MASS::mvrnorm(10, mu = rep(0, 3), Sigma = lav_matrix_lower2full(c(1, .5, 1, .3, .5, 1)))

omitted_sims = specify(cbind(ach, mot, eng) ~
  MASS::mvrnorm(n,
                mu = rep(0, 3),
                Sigma = nearPD(lav_matrix_lower2full(c(1, .5, 1, eng_ach, eng_mot, 1)))$mat)) %>%
  define(n = 100,
         eng_ach = c(0.1, 0.9),
         eng_mot = c(0.1, 0.9)) %>%
  generate(100)

omitted_tidy = omitted_sims %>%
  fit(omitted = ~ lm(ach ~ mot)) %>%  # add test to fits for as.formula, otherwise confusing errror
  tidy_fits

power = omitted_tidy %>%
  group_by(eng_ach, eng_mot) %>%
  summarize(power = mean(p.value < 0.01))

true_reg_coefficient = specify(cbind(ach, mot, eng) ~
                                 MASS::mvrnorm(n,
                                               mu = rep(0, 3),
                                               Sigma = nearPD(lav_matrix_lower2full(c(1, .5, 1, eng_ach, eng_mot, 1)))$mat)) %>%
  define(n = 1000000,
         eng_ach = c(0.1, 0.9),
         eng_mot = c(0.1, 0.9)) %>%
  generate(1) %>%
  fit(true = ~ lm(ach ~ mot + eng)) %>%  # add test to fits for as.formula, otherwise confusing errror
  tidy_fits

true_mot = true_reg_coefficient %>%
  filter(term == "mot") %>%
  select(eng_ach, eng_mot, true_coef = estimate)

bias = omitted_tidy %>%
  group_by(eng_ach, eng_mot) %>%
  filter(term == "mot") %>%
  left_join(true_mot, by = c("eng_ach", "eng_mot")) %>%
  summarize(power = mean(p.value < 0.0001),
            bias = mean(estimate) - mean(true_coef))




