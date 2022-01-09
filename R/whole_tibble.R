#' Convert a simpr_sims object back to a
#' simpr_tibble
#'
#' Undoes \code{\link{per_sim}} to allow access to simulation
#' results as a tibble, with simulations available
#' as a list-column.
#'
#' This function is the inverse of
#' \code{\link{per_sim}}. This enables tidyverse
#' verbs to return to the default behavior of
#' acting on the full table, as opposed to the
#' behavior, activated by \code{\link{per_sim}},
#' of acting elementwise on the simulation
#' results.
#'
#' @param x A \code{simpr_sims} or \code{simpr_spec} object.
#' @return A tibble with the metaparameters and
#'   simulation results
#' @aliases simpr_tibble
#' @export
whole_tibble = function(x) {
 UseMethod("whole_tibble")
}

#' @export
whole_tibble.simpr_sims = function(x) {
  class(x) = setdiff(class(x), "simpr_sims")

  x
}

#' @export
whole_tibble.simpr_tibble = function(x) {
  warning(deparse(substitute(x)), " is already in whole_tibble mode; no change made")
  x
}

#' @export
whole_tibble.simpr_spec = function(x) {
  mc = match.call()
  mc[[1]] = quote(collect)

  x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                     ~ eval(mc))
  x
}

