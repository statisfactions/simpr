## ---- include = FALSE------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE--------------------------------------------------------------------------------------
library(simpr)

## ----data_munging----------------------------------------------------------------------------------------------
specify(control = ~ rnorm(n, mean = 0),
        intervention_1 = ~ rnorm(n, mean = 0.2),
        intervention_2 = ~ rnorm(n, mean = 2)) %>% 
  define(n = c(6, 12)) %>% 
  per_sim() %>% 
  pivot_longer(cols = everything(),
               names_to = "group", 
               values_to = "response") %>% 
  fit(lm = ~ lm(response ~ group)) %>% 
  tidy_fits() %>% 
  select(.sim_id, n, term, estimate) %>% 
  generate(2) 

