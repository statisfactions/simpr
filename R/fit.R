#' Fit the simulated data (from simpr::gen)
#'
#' Takes simulated data (output of simpr::gen) and fits to the specified statistical model.
#'
#' @param simpr_gen simulated data (output of simpr::gen)
#'
#' @return fitted model object of simulated data
#'
#' @examples
#' fit(simpr_gen, lm = ~lm(y ~ x1*x2, data = simpr_gen))
#'
#' @export
fit = function(simpr_gen, ...) {

  fit_functions = list(...)

  simpr_mod = simpr_gen

  for(i in names(fit_functions))
    simpr_mod[[i]] = purrr::map(simpr_mod$sim_cell, fit_functions[[i]])

  attr(simpr_mod, "meta") = attr(simpr_gen, "meta")
  attr(simpr_mod, "variables") = attr(simpr_gen, "variables")
  attr(simpr_mod, "fits") = c(attr(simpr_gen, "fits"), names(fit_functions))

  simpr_mod
}
