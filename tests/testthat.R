library(testthat)
library(simpr)


## Avoid select conflict with MASS
select = dplyr::select

test_check("simpr")
