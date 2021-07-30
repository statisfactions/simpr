#' Fit models to the simulated data
#'
#' Takes simulated data from \code{\link{produce_sims}} and applies functions to it,
#' usually model-fitting functions.
#'
#' This is the fourth step in the simulation process: after generating the
#' simulation data, apply functions such as fitting a statistical model to the
#' data. The output is often then passed to \code{\link{tidy_fits}} or
#' \code{\link{glance_fits}} to extract relevant parameters from the object,
#' based on \code{\link[broom]{tidy}} and \code{\link[broom]{glance}} from the
#' \code{broom} package.
#'
#' Similar to \code{\link{blueprint}}, the \code{\dots} arguments uses an
#' efficient syntax to specify custom functions for fitting models to the data.
#' These functions will usually be on the simulated data -- which is indicated
#' by \code{.} in the formula function, e.g. \code{linear_model = ~lm(y ~ x + z,
#' data = .)} computes linear models on each simulation cell if there are
#' variables x, y, and z specified in \code{blueprint}.
#'
#' @param obj a \code{simpr_blueprint} object--the simulated data from
#'   \code{\link{produce_sims}}--or an \code{\link{include}} object
#' @param ... \code{purrr}-style formula functions used for computing on the
#'   simulated data.  See \emph{Details} and \emph{Examples}.
#'
#' @return a \code{simpr_gen} object with additional list-columns for the output
#'   of the provided functions (e.g. model outputs).  Just like the output of
#'   \code{\link{produce_sims}}, there is one row per repetition per combination of
#'   metaparameters, and the columns are the repetition number \code{rep}, the
#'   metaparameter names, the simulated data \code{sim_cell}, with additional
#'   columns for the function outputs specified in \code{\dots}.
#'
#' @examples
#' ## Generate data to fit models
#' simple_linear_data = blueprint(x1 = ~ 2 + rnorm(n),
#'                                y = ~ 5 + 3*x1 + rnorm(n, 0, sd = 0.5)) %>%
#'   meta(n = 100:101) %>%
#'   produce_sims(2)
#'
#' ## Fit with a single linear term
#' linear_fit = simple_linear_data %>%
#'   fit(linear = ~lm(y ~ x1, data = .))
#'
#' linear_fit
#'
#' ## Each element of $linear is a model object
#' linear_fit$linear
#'
#' ## We can fit multiple models to the same data
#' multi_fit = simple_linear_data %>%
#'   fit(linear = ~lm(y ~ x1, data = .),
#'       quadratic = ~lm(y ~ x1 + I(x1^2), data = .))
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
#' ## We can also use .$colname syntax in fit(), e.g.,:
#' linear_fit = simple_linear_data %>%
#'   fit(linear = ~lm(.$y ~ .$x1)) # no need for "data=."
#'
#' ## Fit functions do not actually need to be any particular kind of model, they
#' ## can be any arbitrary function. However, not all functions will lead to useful
#' ## output with tidy_fits and glance_fits.
#' add_five_data = simple_linear_data %>%
#'   fit(add_five = ~ tibble::as_tibble(. + 5))
#'
#' add_five_data
#'
#' add_five_data$add_five
#'
#' @export
fit = function(obj, ...) {
  UseMethod("fit")
}

#' @export
fit.simpr_produce = function(obj, ...) {

  fit_functions = list(...)

  sim_name = get_sim_name(obj)

  for(i in names(fit_functions))
    obj[[i]] = purrr::map(obj[[sim_name]], fit_functions[[i]])

  # attr(simpr_mod, "meta") = attr(obj, "meta")
  # attr(simpr_mod, "variables") = attr(obj, "variables")
  attr(obj, "fits") = c(attr(obj, "fits"), names(fit_functions))

  obj
}

#' @export
fit.simpr_spec = function(obj, ...) {
  mc = match.call()

  add_call(obj, mc, "fit", replace_arg = "obj")

}

