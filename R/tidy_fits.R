#' Tidy simpr_gen simulated model results output into tibble of components
#'
#' Turn fitted model of simulated data (from \code{\link{fit}}) into a tidy
#' tibble of model components (via \code{generics::\link[generics]{tidy}}).
#'
#' This is part of the fifth step of the simulation process: after fitting the
#' model with \code{\link{fit}}, now tidy the model output for further analysis
#' such as evaluating power.  All model objects should be supported by
#' \code{generics::\link[generics]{tidy}}, primarily via the \code{broom}
#' package.
#'
#' The output of this function is quite useful for calculating things such as
#' power for specific tests within an overall model; see \emph{Examples}. For
#' looking at overall features of the model such as R-squared, use
#' \code{\link{glance_fits}}.
#'
#' @param obj tibble with repetition number, metaparameters, simulated
#'   data, and fitted models, from \code{\link{fit}}
#'
#' @return a tibble with the output of the
#'   \code{generics::\link[generics]{tidy}} method for the given object.
#'
#' @seealso \code{\link{glance_fits}} to view overall model statistics (e.g.
#'   R-squared)
#' @examples
#' simple_linear_data = blueprint(x1 = ~ 2 + rnorm(n),
#'           y = ~ 5 + 3 * x1 + rnorm(n, 0, sd = 0.5)) %>%
#'   meta(n = 100:101) %>%
#'   produce_sims(2)
#'
#' ## Can show tidy output for multiple competing models,
#' compare_degree = simple_linear_data %>%
#'   fit(linear = ~lm(y ~ x1, data = .),
#'       quadratic = ~lm(y ~ x1 + I(x1^2), data = .)) %>%
#'   tidy_fits
#'
#' ## Models can be of different types -- anything supported by broom::tidy.
#' cor_vs_lm = simple_linear_data %>%
#'   fit(linear = ~lm(y ~ x1, data = .),
#'       cor = ~ cor.test(.$y, .$x1)) %>%
#'   tidy_fits
#'
#' cor_vs_lm # has NA for non-matching terms
#'
#' ## Example power analysis to detect an interaction (g1)
#' \donttest{
#' set.seed(100)
#' simpr_tidy = ## Specify the simulation
#'   blueprint(x1 = ~ 2 + rnorm(n),
#'             x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
#'             y = ~ 5 + x1 + x2 + g1*x1*x2 + 10 * rnorm(n)) %>%
#'   meta(n = seq(100, 300, by = 20),
#'        g1 = seq(-1, 1, by = 0.5)) %>%
#'   ## Generate the data
#'   produce_sims(10) %>%
#'   ## Fit models
#'   fit(lm = ~lm(y ~ x1*x2, data = .)) %>%
#'   ## Calculate the output
#'   tidy_fits
#'
#' ## Now we can easily calculate and plot power
#' library(dplyr)
#' library(ggplot2)
#' simpr_tidy %>%
#'   filter(term %in% "x1:x2") %>%
#'   group_by(n, g1) %>%
#'   summarize(power = mean(p.value < 0.05)) %>%
#'   ggplot(aes(n, power)) +
#'   geom_line() +
#'   facet_grid(~g1)
#' }
#' @export
tidy_fits = function(obj) {
  ## Run broom::tidy() on fit columns in simpr_mod
  apply_fits(obj, broom::tidy)
}



