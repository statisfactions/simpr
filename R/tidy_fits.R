#' Tidy fits into a tidy tibble
#'
#' Turn models fit to simulated data (from
#' \code{\link[=fit.simpr_tibble]{fit}}) into a
#' tidy tibble of model estimates (via
#' \code{broom::\link[generics]{tidy}}).
#'
#' This the fifth step of the simulation process:
#' after fitting the model with
#' \code{\link[=fit.simpr_tibble]{fit}}, now tidy
#' the model output for further analysis such as
#' evaluating power.  All model objects should be
#' supported by
#' \code{broom::\link[generics]{tidy}}. See
#' \code{\link{apply_fits}} for applying any
#' arbitrary function to the data, including other
#' tidiers.
#'
#' The output of this function is quite useful for
#' diagnosing bias, precision, and power. For
#' looking at overall features of the model (e.g.,
#' R-squared), use \code{\link{glance_fits}}.
#'
#' @param obj a \code{simpr_tibble} with fitted
#'   models, from
#'   \code{\link[=fit.simpr_tibble]{fit}}
#' @param \dots Additional arguments to the
#'   \code{broom::\link[generics]{tidy}} method.
#' @inheritParams fit.simpr_tibble
#' @return a tibble with the output of the
#'   \code{broom::\link[generics]{tidy}} method
#'   applied to each model fit and then bound into
#'   a single tibble.
#'
#' @seealso \code{\link{glance_fits}} to view
#'   overall model statistics (e.g. R-squared),
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
#'   tidy_fits
#'
#' compare_degree
#'
#' ## Models can be anything supported by broom::tidy.
#' cor_vs_lm = simple_linear_data %>%
#'   fit(linear = ~lm(a ~ b, data = .),
#'       cor = ~ cor.test(.$a, .$b)) %>%
#'   tidy_fits
#'
#' cor_vs_lm # has NA for non-matching terms
#' @export
tidy_fits = function(obj, ..., .progress = FALSE,
                     .options = furrr_options()) {
  ## Run broom::tidy() on fit columns in simpr_mod
  apply_fits(obj, broom::tidy, ...,
             .progress = .progress,
             .options = .options)
}



