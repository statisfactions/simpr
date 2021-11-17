#' Tidy simpr_gen simulated model results output into tibble of model summaries
#'
#' Turn fitted model of simulated data (from \code{\link{fit}}) into a tidy
#' tibble of model summaries (via \code{generics::\link[generics]{glance}}).
#'
#' This is part of the fifth step of the simulation process: after fitting the
#' model with \code{\link{fit}}, now tidy the model output for further analysis
#' such as evaluating power.  All model objects should be supported by
#' \code{generics::\link[generics]{glance}}, primarily via the \code{broom}
#' package.
#'
#' The output of this function is quite useful for calculating things overall
#' model fit; see \emph{Examples}. For looking at specific features of the model
#' such as tests for individual parameter estimates, use \code{\link{tidy_fits}}.
#'
#' @param obj tibble with repetition number, metaparameters, simulated
#'   data, and fitted models, from \code{\link{fit}}
#' @param \dots Additional arguments to \code{generics::glance}.
#' @inheritParams fit
#' @return a tibble with the output of the
#'   \code{generics::\link[generics]{glance}} method for the given object.
#'
#' @seealso \code{\link{tidy_fits}} to view model components (e.g.
#'   parameter estimates),
#'   \code{\link{apply_fits}} to apply an
#'   arbitrary function to the fits
#' @examples
#' simple_linear_data = specify(x1 = ~ 2 + rnorm(n),
#'           y = ~ 5 + 3 * x1 + rnorm(n, 0, sd = 0.5)) %>%
#'   define(n = 100:101) %>%
#'   generate(2)
#'
#' ## Can show glance output for multiple competing models,
#' compare_degree = simple_linear_data %>%
#'   fit(linear = ~lm(y ~ x1, data = .),
#'       quadratic = ~lm(y ~ x1 + I(x1^2), data = .)) %>%
#'   glance_fits
#'
#' ## Models can be of different types -- anything supported by broom::glance.
#' cor_vs_lm = simple_linear_data %>%
#'   fit(linear = ~lm(y ~ x1, data = .),
#'       cor = ~ cor.test(.$y, .$x1)) %>%
#'   glance_fits
#'
#' cor_vs_lm # has NA for non-matching terms
#'
#' ## Example power analysis to detect an interaction (g1)
#' \donttest{set.seed(100)
#' simpr_glance = ## Specify the simulation
#'   specify(x1 = ~ 2 + rnorm(n),
#'             x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
#'             y = ~ 5 + x1 + x2 + g1*x1*x2 + 10 * rnorm(n)) %>%
#'   define(n = seq(100, 300, by = 20),
#'        g1 = seq(-1, 1, by = 0.5)) %>%
#'   ## Generate the data
#'   generate(10) %>%
#'   ## Fit models
#'   fit(lm = ~lm(y ~ x1*x2, data = .)) %>%
#'   ## Calculate the output
#'   glance_fits
#'
#' ## Now we can easily calculate and plot r.squared by model
#' library(dplyr)
#' library(ggplot2)
#' simpr_glance %>%
#'   group_by(n, g1) %>%
#'   summarize(mean_r_squared = mean(r.squared)) %>%
#'   ggplot(aes(n, mean_r_squared)) +
#'   geom_line() +
#'   facet_grid(~g1) +
#'   coord_cartesian(ylim = c(0,1))
#' }
#' @export
glance_fits = function(obj, ..., .progress = FALSE,
                       .options = furrr_options()) {
  ## Run broom::glance() on fit columns in simpr_mod
  apply_fits(obj, broom::glance, ...,
             .progress = .progress,
             .options = .options)
}



