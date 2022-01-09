#' Create tibble of model "glances" (summaries)
#'
#' Turn fitted models of simulated data (from
#' \code{\link[=fit.simpr_tibble]{fit}}) into a
#' tidy tibble of model summaries, each with one line (via
#' \code{broom::\link[generics]{glance}}).
#'
#' This the fifth step of the
#' simulation process: after fitting the model
#' with \code{\link[=fit.simpr_tibble]{fit}}, now
#' tidy the model output for further analysis such
#' as evaluating power.  All model objects should
#' be supported by
#' \code{broom::\link[generics]{glance}}.
#'
#' The output of this function is quite useful comparing
#'  overall model fits; see
#' \emph{Examples}. For looking at specific
#' features of the model such as tests for
#' individual parameter estimates, use
#' \code{\link{tidy_fits}}.
#'
#' @param obj tibble with repetition number,
#'   metaparameters, simulated data, and fitted
#'   models, from
#'   \code{\link[=fit.simpr_tibble]{fit}}
#' @param \dots Additional arguments to
#'   \code{broom::\link[generics]{glance}}.
#' @inheritParams fit.simpr_tibble
#' @return a tibble with the output of the
#'   \code{broom::\link[generics]{glance}}
#'   method for the given object.
#'
#' @seealso \code{\link{tidy_fits}} to view model
#'   components (e.g. parameter estimates),
#'   \code{\link{apply_fits}} to apply an
#'   arbitrary function to the fits
#' @examples
#' simple_linear_data = specify(a = ~ 2 + rnorm(n),
#'           b = ~ 5 + 3 * x1 + rnorm(n, 0, sd = 0.5)) %>%
#'   define(n = 100:101) %>%
#'   generate(2)
#'
#' ## Can show tidy output for multiple competing models,
#' compare_degree = simple_linear_data %>%
#'   fit(linear = ~lm(a ~ b, data = .),
#'       quadratic = ~lm(a ~ b + I(b^2), data = .)) %>%
#'   glance_fits
#'
#' compare_degree
#'
#' ## Models can be anything supported by broom::tidy.
#' cor_vs_lm = simple_linear_data %>%
#'   fit(linear = ~lm(a ~ b, data = .),
#'       cor = ~ cor.test(.$a, .$b)) %>%
#'   glance_fits
#'
#' cor_vs_lm # has NA for non-matching terms
#' @export
glance_fits = function(obj, ..., .progress = FALSE,
                       .options = furrr_options()) {
  ## Run broom::glance() on fit columns in simpr_mod
  apply_fits(obj, broom::glance, ...,
             .progress = .progress,
             .options = .options)
}



