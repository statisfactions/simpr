#' Specify functions to generate variables
#'
#' Creates a simpr class object that specifies formulae for computing each variable in the model.
#'
#' @return simpr class object
#'
#' @examples
#' variables(x1 = ~ 2 + rnorm(n),
#'     x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
#'     y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = s))
#'
#' @export
variables = function(...) {
  out = list(variables = list(...))
  class(out) = "simpr"
  out
}

#' Specify metaparameters to vary in simulation
#'
#' Takes a simpr class object (e.g., from variables()) and defines the metaparameters for simulation.
#'
#' @param x simpr class object (e.g., output of variables())
#'
#' @return simpr class object
#'
#' @examples
#' variables(x1 = ~ 2 + rnorm(n),
#'     x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
#'     y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = s)) \%>\%
#'         meta(n = seq(100, 300, by = 20),
#'              b1 = 1,
#'              b2 = 1,
#'              g1 = seq(-1, 1, by = 0.5),
#'              s = seq(0.2, 50, length.out = 6))
#'
#' @export
meta = function(x, ...) {
  x$meta = list(...)
  x
}

#' Generate simulated data from specified simpr model (from simpr::variables \%>\% simpr::meta())
#'
#' Takes a simpr class object with metaparameters (e.g., output of simpr::variables() \%>\% simpr::meta()) and generates simulated data to be fit with a statistical model (e.g., lm().
#'
#' @param simpr simpr class object with metaparamters (e.g., output of simpr::variables() \%>\% simpr::meta())
#' @param reps number of replications to run (integer)
#'
#' @examples
#' gen(simpr, reps = 1000)
#'
#' @return Simulated data
#'
#' @export
gen = function(simpr, reps) {
  # Create labeled list representing all possible values of meta parameters
  specs = expand.grid(c(simpr$meta, list(rep = 1:reps)))

  ## Generate all replications
  sim_results = specs %>%
    dplyr::group_by_all() %>%
    dplyr::do(sim_cell = purrr::pmap(., function(...) {
      meta_cell = list(...)
      attach(meta_cell)

      df = purrr::imap_dfc(simpr$variables, function(x, y) {

        eval_fn = purrr::as_mapper(x)

        gen = eval_fn() %>%
          unlist

        assign(y, gen, envir = .GlobalEnv)
        gen
      })

      detach(meta_cell)
      df

    })) %>% tidyr::unnest()
  attr(sim_results, "meta") = names(simpr$meta)
  attr(sim_results, "variables") = names(simpr$variables)

  sim_results
}


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
    simpr_mod[[i]] = purrr::map(simpr_mod$sim_cell, fit_functions[[1]])

  attr(simpr_mod, "meta") = attr(simpr_gen, "meta")
  attr(simpr_mod, "variables") = attr(simpr_gen, "variables")
  attr(simpr_mod, "fits") = c(attr(simpr_gen, "fits"), names(fit_functions))

  simpr_mod
}

# fit_lm = function(simpr_gen, ...) {
#   ## ... = arguments to lm
#   simpr_mod = simpr_gen %>%
#     group_by_at(c(attr(simpr_gen, "meta"), "rep")) %>%
#     do(mod = lm(data = ., ...))
#
#   attr(simpr_mod, "meta") = attr(simpr_gen, "meta")
#   attr(simpr_mod, "variables") = attr(simpr_gen, "variables")
#
#   simpr_mod
# }

#' Tidy the simulated model results output into tibble of components (broom analogue)
#'
#' Turn fitted model of simulated data (output of simpr::fit) into a tidy tibble of model components.
#'
#' @param simpr_mod simulated model results (output of simpr::fit)
#'
#' @return tidied model components of fitted simulated data
#'
#' @examples
#' calc_tidy(simpr_mod)
#'
#' @export
calc_tidy = function(simpr_mod) {
  ## Create reference meta df for merging
  simpr_meta = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "meta"), "rep"))) %>%
    dplyr::mutate(....id = as.character(1:(dplyr::n())))

  ## Extract all fit columns
  simpr_mods = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$....id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, ~ purrr::map_dfr(., broom::tidy, .id = "....id"), .id = "Source")

  ## Re-merge metaparameter columns to tidy output
  dplyr::right_join(simpr_meta, simpr_tidy, by = "....id") %>%
    dplyr::select(-....id)

}

#' Glance at the simulated model results output (broom analogue)
#'
#' Turn fitted model of simulated data (output of simpr::fit) into a tidy tibble of overall model statistics
#'
#' @param simpr_mod simulated model results (output of simpr::fit)
#'
#' @return tidied model summary of fitted simulated data
#'
#' @examples
#' calc_glance(simpr_mod)
#'
#' @export
calc_glance = function(simpr_mod) {
  simpr_meta = simpr_mod %>% dplyr::select(-mod)

  simpr_glance = simpr_mod %>%
    do(glance = broom::glance(.$mod)) %>%
    dplyr::bind_cols(simpr_meta, .) %>%
    tidyr::unnest()

}
