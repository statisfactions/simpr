#' Convert a simpr_sims object back to a
#' simpr_tibble or bare tibble
#'
#' Allows access to simulation results as a
#' tibble, with simulations available as a
#' list-column.
#'
#' This function is the inverse of
#' \code{\link{as_sims}}.  This enables
#' tidyverse verbs to act on the full table as
#' opposed to the default behavior of acting
#' elementwise on the simulation results. If
#' \code{bare_tibble = TRUE}, the output is a bare
#' tibble object that will not work with simpr
#' methods such as \code{\link{fit}}.
#'
#' @param x A \code{simpr_sims} or
#'   \code{simpr_tibble} object.
#' @param ... Ignored.
#' @param bare_tibble Convert to a bare tibble
#' @param .rows Ignored.
#' @param .name_repair Ignored.
#' @param rownames Ignored.
#' @export
as_tibble.simpr_sims = function(x,..., bare_tibble = FALSE, .rows, .name_repair, rownames) {
  class(x) = setdiff(class(x), "simpr_sims")

  if(!bare_tibble)
    x
  else {
    class(x) = setdiff(class(x), "simpr_tibble")
    x
  }

}

#' @export
as_tibble.simpr_tibble = function(x, ..., bare_tibble = FALSE, .rows, .name_repair, rownames) {
  if(!bare_tibble)
    x
  else {
    class(x) = setdiff(class(x), "simpr_tibble")
    x
  }
}

#' @export
as_tibble.simpr_spec = function(x,..., .rows, .name_repair, rownames) {
  mc = match.call()
  mc[[1]] = quote(collect)

  x[[get_sim_name(x)]] =  purrr::map(x[[get_sim_name(x)]],
                                     ~ eval(mc))
  x
}

#' @importFrom tibble as_tibble
#' @export
tibble::as_tibble

