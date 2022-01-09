#' Fit models to the simulated data
#'
#' Takes simulated data from
#' \code{\link[=generate.simpr_spec]{generate}}
#' and applies functions to it, usually
#' model-fitting functions.
#'
#' This is the fourth step in the simulation
#' process: after generating the simulation data,
#' apply functions such as fitting a statistical
#' model to the data. The output is often then
#' passed to \code{\link{tidy_fits}} or
#' \code{\link{glance_fits}} to extract relevant
#' model estimates from the object.
#'
#' Similar to
#' \code{\link[=specify.formula]{specify}}, the
#' model-fitting \code{\dots} arguments can be
#' arbitrary R expressions (\code{purrr}-style
#' lambda functions, see
#' \code{\link[purrr]{as_mapper}}) to specify
#' fitting models to the data. The functions are
#' computed within each simulation cell, so
#' dataset names are generally unnecessary: e.g.,
#' to compute regressions on each cell,
#' \code{fit(linear_model = ~ lm(c ~ a + b)}.  If
#' your modeling function requires a reference to
#' the full dataset, use \code{.}, e.g.
#' \code{fit(linear_model = ~lm(c ~ a + b, data =
#' .)}.
#'
#' @param object a \code{simpr_tibble} object--the
#'   simulated data from
#'   \code{\link[=generate.simpr_spec]{generate}}--or
#'   an \code{simpr_spec} object not yet
#'   generated.
#' @param ... \code{purrr}-style lambda functions
#'   used for computing on the simulated data. See
#'   \emph{Details} and \emph{Examples}.
#' @param .quiet Should simulation errors be
#'   broadcast to the user as they occur?
#' @param .warn_on_error Should there be a warning
#'   when simulation errors occur? See
#'   \code{vignette("Managing simulation errors")}.
#' @param .stop_on_error Should the simulation
#'   stop immediately when simulation errors
#'   occur?
#' @param .debug Run simulation in debug mode,
#'   allowing objects, etc. to be explored for
#'   each attempt to fit objects.
#' @param .progress	A logical, for whether or not
#'   to print a progress bar for multiprocess,
#'   multisession, and multicore plans .
#' @param .options The \code{future} specific
#'   options to use with the workers when using
#'   futures. This must be the result from a call
#'   to
#'   \code{\link[furrr:furrr_options]{furrr_options()}}.
#'
#' @return a \code{simpr_tibble} object with
#'   additional list-columns for the output of the
#'   provided functions (e.g. model outputs). Just
#'   like the output of
#'   \code{\link[=generate.simpr_spec]{generate}},
#'   there is one row per repetition per
#'   combination of metaparameters, and the
#'   columns are the repetition number \code{rep},
#'   the metaparameter names, the simulated data
#'   \code{sim}, with additional columns for the
#'   function outputs specified in \code{\dots}.
#'   If \code{\link{per_sim}} was called
#'   previously, \code{fit} returns the object to
#'   default \code{simpr_tibble} mode.
#'
#' @examples
#' ## Generate data to fit models
#' simple_linear_data = specify(a = ~ 2 + rnorm(n),
#'                                b = ~ 5 + 3*a + rnorm(n, 0, sd = 0.5)) %>%
#'   define(n = 100:101) %>%
#'   generate(2)
#'
#' ## Fit with a single linear term
#' linear_fit = simple_linear_data %>%
#'   fit(linear = ~lm(b ~ a, data = .))
#'
#' linear_fit # first fit element also prints
#'
#' ## Each element of $linear is a model object
#' linear_fit$linear
#'
#' ## We can fit multiple models to the same data
#' multi_fit = simple_linear_data %>%
#'   fit(linear = ~lm(b ~ a, data = .),
#'       quadratic = ~lm(b ~ a + I(a^2), data = .))
#'
#' ## Two columns, one for each model
#' multi_fit
#'
#' ## Again, each element is a model object
#' multi_fit$quadratic
#'
#' ## Can view terms more nicely with tidy_fits
#' multi_fit %>%
#'   tidy_fits
#'
#' ## Can view model summaries with glance_fits
#' multi_fit %>%
#'   glance_fits
#'
#' ## Fit functions do not actually need to be any particular kind of model, they
#' ## can be any arbitrary function. However, not all functions will lead to useful
#' ## output with tidy_fits and glance_fits.
#' add_five_data = simple_linear_data %>%
#'   fit(add_five = ~ . + 5)  ## adds 5 to every value in dataset
#'
#' add_five_data
#'
#' @export
fit.simpr_tibble = function(object, ...,
                            .quiet = TRUE, .warn_on_error = TRUE,
                            .stop_on_error = FALSE,
                            .debug = FALSE, .progress = FALSE,
                            .options = furrr_options()) {

  to_fit_fn = function(formula, .debug, .stop_on_error, .quiet) {
    stopifnot(rlang::is_formula(formula))
    afl = rlang::as_function(formula) %>% as.list
    afl[[5]] = call("with", data = quote(.), expr = afl[[5]])
    alt_fn = as.function(afl)

    if(!.stop_on_error && !.debug)
      alt_fn = purrr::safely(alt_fn, quiet = .quiet)

    if(.debug)
      debug(alt_fn)

    return(alt_fn)

  }

  if("simpr_sims" %in% class(object))
    object = whole_tibble(object)



  fit_formulas = list(...)
  sim_name = get_sim_name(object)

  fit_out = purrr::imap(fit_formulas, ~ furrr::future_map(object[[sim_name]],
                                               to_fit_fn(.x, .debug = .debug,
                                                         .stop_on_error = .stop_on_error,
                                                         .quiet = .quiet),
                                               .progress = .progress,
                                               .options = .options))

  any_fit_error = FALSE
  for(i in names(fit_out)) {
    object[[i]] = purrr::map(fit_out[[i]], "result")

    errors = purrr::map(fit_out[[i]], "error")

    if(!all(purrr::map_lgl(errors, is.null))) {
      any_fit_error = TRUE
      object[[paste0(".fit_error_", i)]] = purrr::map(errors,
                                                   ~ ifelse(is.null(.x),
                                                            NA_character_,
                                                            as.character(.x))) %>%
        unlist
    }
  }

  # attr(simpr_mod, "meta") = attr(object, "meta")
  # attr(simpr_mod, "variables") = attr(object, "variables")
  attr(object, "fits") = c(attr(object, "fits"), names(fit_formulas))

  ## Give warning if errors occured
  if(.warn_on_error && any_fit_error)
    warning("fit() produced errors.  See '.fit_error_*' column(s).")

  object
}

#' @export
#' @rdname fit.simpr_tibble
fit.simpr_spec = function(object, ...,  .quiet = TRUE, .warn_on_error = TRUE,
                          .stop_on_error = FALSE,
                          .debug = FALSE, .progress = FALSE,
                          .options = furrr_options()) {
  mc = match.call()

  add_call(object, mc, "fit", replace_arg = "object")

}

#' @importFrom generics fit
#' @export
generics::fit

