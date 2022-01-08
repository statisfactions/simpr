## ---- include = FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup-----------------------------------------------------------------------------------------------------
library(simpr)

## ----run_1-----------------------------------------------------------------------------------------------------
set.seed(500)
run_1 = specify(a = ~ runif(6)) %>% 
  generate(3)

run_1

## ----run_2-----------------------------------------------------------------------------------------------------
set.seed(500)
run_2 = specify(a = ~ runif(6)) %>% 
  generate(3)

run_2

## ----run_compare-----------------------------------------------------------------------------------------------
identical(run_1, run_2)

## ----filter_after_generating-----------------------------------------------------------------------------------
set.seed(500)
filter_after_generating = specify(a = ~ runif(6)) %>% 
  generate(3) %>% 
  filter(.sim_id == 2)

filter_after_generating

## ----filter_while_generating-----------------------------------------------------------------------------------
## Much faster, same result!
set.seed(500)
filter_while_generating = specify(a = ~ runif(6)) %>% 
  generate(3, .sim_id == 2)

filter_while_generating

## ----filter_test-----------------------------------------------------------------------------------------------
identical(filter_after_generating, filter_while_generating)

## ----filter_max_10---------------------------------------------------------------------------------------------
set.seed(500)
filter_max_10 = specify(a = ~ sample(0:max, size = 10, replace = TRUE),
        b = ~ a + rnorm(10))  %>% 
  define(max = c(0, 1, 10)) %>%
  generate(3, max == 10)

filter_max_10

## ----fit_error_data--------------------------------------------------------------------------------------------
fit_errors = filter(fit_tidy, !is.na(.fit_error))

set.seed(500)
fit_error_data = specify(a = ~ sample(1:max, size = 10, replace = TRUE),
                     b = ~ a + rnorm(10))  %>% 
  define(max = c(0, 1, 10)) %>%
  generate(3, .sim_id %in% fit_errors$.sim_id)

fit_error_data

