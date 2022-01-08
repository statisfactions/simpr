## ---- include = FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-----------------------------------------------------------------------------------------------------
library(simpr)
set.seed(2001)

## ----correct_order---------------------------------------------------------------------------------------------
specify(a = ~ runif(6), 
        b = ~ a + rnorm(6)) %>% 
  generate(1)


## ----correct_number--------------------------------------------------------------------------------------------
specify(a = ~ runif(6), 
        b = ~ rnorm(6)) %>% 
  generate(1)

## ----recycle_number--------------------------------------------------------------------------------------------
specify(a = ~ runif(1), 
        b = ~ rnorm(6)) %>% 
  generate(1)

## ----x_error_fixed---------------------------------------------------------------------------------------------
specify(y = ~ runif(6),
        a = ~ y + runif(6)) %>% 
  generate(1)

## ----multicolumn_default---------------------------------------------------------------------------------------
specify(a = ~ MASS::mvrnorm(6, 
                            mu = rep(0, 3),
                            Sigma = diag(rep(1, 3)))) %>% 
  generate(1)


## ----multicolumn_sep-------------------------------------------------------------------------------------------
specify(a = ~ MASS::mvrnorm(6, 
                            mu = rep(0, 3),
                            Sigma = diag(rep(1, 3))),
        .sep = ".") %>% 
  generate(1)


## ----multicolumn_two_sided-------------------------------------------------------------------------------------
specify(y = c(a, b, c) ~ MASS::mvrnorm(6, 
                            mu = rep(0, 3),
                            Sigma = diag(rep(1, 3)))) %>% 
  generate(1)


## ----multicolumn_.use_names------------------------------------------------------------------------------------
specify(a = ~ MASS::mvrnorm(6, 
                            mu = rep(0, 3),
                            Sigma = diag(rep(1, 3))) %>% 
          magrittr::set_colnames(c("d", "e", "f"))) %>% 
  generate(1)

## ----multicolumn_refer-----------------------------------------------------------------------------------------
specify(a = ~ MASS::mvrnorm(6, 
                            mu = rep(0, 3),
                            Sigma = diag(rep(1, 3))),
        b = ~ a_1 - a_2) %>% 
  generate(1)

## ----define_n--------------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n)) %>% 
  define(n = c(10, 20)) %>% 
  generate(1)

## ----define_n_mu-----------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n, mu)) %>% 
  define(n = c(10, 20),
         mu = c(0, 10)) %>% 
  generate(1)

## ----define_matrix---------------------------------------------------------------------------------------------

specify(a = ~ MASS::mvrnorm(6, rep(0, 2), Sigma = s)) %>% 
  define(s = list(independent = diag(rep(1, 2)),
                  dependent = matrix(c(1, 0.5, 0.5, 1), nrow = 2))) %>% 
  generate(1)


## ----define_function-------------------------------------------------------------------------------------------
specify(y = ~ distribution(6)) %>%
  define(distribution = list(normal = rnorm,
                             lognormal = rlnorm)) %>%
  generate(1)

## ----generate_n_mu_2-------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n, mu)) %>% 
  define(n = c(6, 12),
         mu = c(0, 10)) %>% 
  generate(2)

## ----generate_filter-------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n, mu)) %>% 
  define(n = c(6, 12),
         mu = c(0, 10)) %>% 
  generate(2, n > mu)

## ----fit_initial-----------------------------------------------------------------------------------------------
specify(a = ~ rnorm(6),
        b = ~ a + rnorm(6)) %>% 
  generate(1) %>% 
  fit(t_test = ~ t.test(a, b),
      lm = ~ lm(b ~ a))

## ----fit_describe----------------------------------------------------------------------------------------------
specify(a = ~ rnorm(6)) %>% 
  generate(1) %>% 
  fit(mean = ~ mean(a),
      why_not = ~ a + 5)



## ----fit_explicit----------------------------------------------------------------------------------------------
specify(a = ~ rnorm(6),
        b = ~ a + rnorm(6)) %>% 
  generate(1) %>% 
  fit(t_test = ~ t.test(.$a, .$b),
      lm = ~ lm(b ~ a, data = .))

## ----fit_reshape_1---------------------------------------------------------------------------------------------
wide_gen = specify(control = ~ rnorm(6, mean = 0),
        intervention_1 = ~ rnorm(6, mean = 0.2),
        intervention_2 = ~ rnorm(6, mean = 2)) %>% 
  generate(2) 

wide_gen

## ----fit_reshape_success---------------------------------------------------------------------------------------
long_gen = wide_gen %>%  
  per_sim() %>% 
  pivot_longer(cols = everything(),
               names_to = "group", 
               values_to = "response")

long_gen

## ----long_fit--------------------------------------------------------------------------------------------------
long_fit = long_gen %>% 
  fit(aov = ~ aov(response ~ group),
      lm = ~ lm(response ~ group))

long_fit

## ----tidy_fits_simple------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n),
        b = ~ a + rnorm(n)) %>% 
  define(n = c(6, 12)) %>% 
  generate(2) %>% 
  fit(lm = ~ lm(b ~ a)) %>% 
  tidy_fits()


## ----tidy_fits_complex-----------------------------------------------------------------------------------------
specify(a = ~ rnorm(n),
        b = ~ a + rnorm(n)) %>% 
  define(n = c(6, 12)) %>% 
  generate(2) %>% 
  fit(lm = ~ lm(b ~ a),
      t_test = ~ t.test(a, b)) %>% 
  tidy_fits()


## ----tidy_fits_custom------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n),
        b = ~ a + rnorm(n)) %>% 
  define(n = c(6, 12)) %>% 
  generate(2) %>% 
  fit(lm = ~ lm(b ~ a),
      t_test = ~ t.test(a, b)) %>% 
  tidy_fits(conf.level = 0.99, conf.int = TRUE)


## ----glance_fits_simple----------------------------------------------------------------------------------------
specify(a = ~ rnorm(n),
        b = ~ a + rnorm(n)) %>% 
  define(n = c(6, 12)) %>% 
  generate(2) %>% 
  fit(lm = ~ lm(b ~ a)) %>% 
  glance_fits()


## ----apply_fits------------------------------------------------------------------------------------------------
specify(a = ~ rnorm(n),
        b = ~ a + rnorm(n)) %>% 
  define(n = c(6, 12)) %>% 
  generate(2) %>% 
  fit(lm = ~ lm(b ~ a)) %>% 
  apply_fits(~ summary(.)$r.squared)

