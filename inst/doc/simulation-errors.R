## ---- include = FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE--------------------------------------------------------------
library(simpr)

## ----buggy_spec--------------------------------------------------------------------------
buggy_spec = specify(y = ~ rnorm(size)) %>%
  define(size = c(-10, 10))


## ----generate_no_warn--------------------------------------------------------------------
set.seed(100)
generate_no_warn = buggy_spec %>% 
  generate(1, .warn_on_error = FALSE)

generate_no_warn

## ----generate_no_warn_output-------------------------------------------------------------
generate_no_warn

