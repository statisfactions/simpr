#' Work directly with simulation results with
#' dplyr and tidyr
#'
#' Allows applying data transformations to every
#' simulation result with syntax as if dealing
#' with a single simulation result using dplyr and
#' tidyr verbs
#'
#' After producing simulation results (a
#' \code{simpr_tibble} object), it is sometimes
#' needed to do some data transformation to
#' prepare for analysis.  This can always be
#' specified in specify through custom
#' functions, but \code{per_sim} allows you to
#' also easily specify this in your pipeline.
#' After running per_sim, you can use the dplyr
#' and tidyr verbs you would use on a single
#' simulation result and it will be applied to all
#' results.
#'
#' If, after running \code{per_sim}, you wish to
#' return to the default behavior to access
#' \code{simpr_tibble} results as a tibble with a
#' list_column for simulation results again, run
#' \code{\link{whole_tibble}}.
#'
#' @param obj A \code{simpr_tibble} or \code{simpr_spec} object.
#' @return A \code{simpr_sims} object for use with dplyr and tidyr verbs.
#' @aliases simpr_sims
#' @examples
#' ## Often most convenient to specify simulations for 'wide' data
#' data_wide = specify(a = ~ runif(5, min = 0, max = 1),
#'                     b = ~ runif(5, min = 0, max = 2)) %>%
#'   generate(2)
#'
#' data_wide
#'
#' ## Any dplyr or tidyr verbs can be applied after per_sim()
#' data_long = data_wide %>%
#'   per_sim() %>%
#'   pivot_longer(everything(), names_to = "name",
#'                values_to = "value")
#' data_long
#'
#' ## Now, ready for analysis
#' data_long %>%
#'   fit(lm = ~lm(value ~ name)) %>%
#'   tidy_fits
#'
#'
#' @export
per_sim = function(obj) {
 UseMethod("per_sim")
}

#' @export
per_sim.simpr_tibble = function(obj) {
  class(obj) = c("simpr_sims", class(obj))
  obj
}

#' @export
per_sim.simpr_spec = function(obj) {
  mc = match.call()

  add_call(obj, mc, "per_sim", replace_arg = "obj")

}

