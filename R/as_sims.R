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
#' specified in blueprint through custom
#' functions, but \code{as_sims} allows you to
#' also easily specify this in your pipeline.
#' After running as_sims, you can use the dplyr
#' and tidyr verbs you would use on a single
#' simulation result and it will be applied to all
#' results.
#'
#' If, after running \code{as_sims}, you wish to
#' return to the default behavior to access
#' \code{simpr_tibble} results as a tibble with a
#' list_column for simulation results again, run
#' \code{as_tibble()}.
#'
#' @param obj A \code{simpr_tibble} or \code{simpr_spec} object.
#' @return A \code{simpr_sims} object for use with dplyr and tidyr verbs.
#' @aliases simpr_sims
#' @export
as_sims = function(obj) {
 UseMethod("as_sims")
}

#' @export
as_sims.simpr_tibble = function(obj) {
  class(obj) = c("simpr_sims", class(obj))
  obj
}

#' @export
as_sims.simpr_spec = function(obj) {
  mc = match.call()

  add_call(obj, mc, "as_sims", replace_arg = "obj")

}

