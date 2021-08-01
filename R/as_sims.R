#' Work directly with simulation results with
#' dplyr and tidyr
#'
#' Allows applying data transformations to every
#' simulation result with syntax as if dealing
#' with a single simulation result using dplyr and
#' tidyr verbs
#'
#' After producing simulation results (a
#' \code{simpr_produce} object), it is sometimes
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
#' return to the default behavoir to access
#' \code{simpr_produce} results as a tibble with a
#' list_column for simulation results again, run
#' \code{as_tibble()}.
#'
#' @param obj A \code{simpr_produce} or \code{simpr_spec} object.
#' @return A \code{simpr_sims} object for use with dplyr and tidyr verbs.
#' @export
as_sims = function(obj) {
 UseMethod("as_sims")
}

#' @export
as_sims.simpr_produce = function(obj) {
  class(obj) = c("simpr_sims", class(obj))
  obj
}

as_sims.simpr_spec = function(obj) {
  mc = match.call()

  add_call(obj, mc, "as_sims", replace_arg = "obj")

}

#' @export
as_tibble.simpr_sims = function(obj) {
  class(obj) = setdiff(class(obj), "simpr_sims")
  obj
}

#' @export
mutate.simpr_sims = function(.data, ...) {
  .data[[get_sim_name(.data)]] =  purrr::map(.data[[get_sim_name(.data)]],
                                             mutate, ...)
  .data
}

