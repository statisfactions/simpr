#' Run a given function on a simpr_mod object and
#' tidy the output.
#'
#' This function applies a given function to all
#' fit objects and returns the result in a tidy
#' tibble.  Any function or \code{purrr}-style
#' lambda function can be used.
#'
#'
#' @param obj tibble with repetition number,
#'   metaparameters, simulated data, and fitted
#'   models, from \code{\link{fit}}
#' @param .f A function or \code{purrr}-style formula function used for computing on the
#'   fit object
#' @param \dots Additional arguments to \code{.f}.
#' @seealso \code{\link{tidy_fits}}, \code{\link{glance_fits}}
#' @export
apply_fits = function(obj, .f, ...) {
  UseMethod("apply_fits")
}


#' @export
apply_fits.simpr_spec = function(obj, .f, ...) {
  mc = match.call()

  add_call(obj, mc, "apply_fits", replace_arg = "obj")
}

#' @export
apply_fits.simpr_tibble = function(obj, .f, ...) {

  fn_map = function(...) dplyr::as_tibble(purrr::as_mapper(.f)(...))
  ## Create reference meta df for merging
  simpr_meta = obj %>%
    dplyr::select(tidyselect::one_of(c(attr(obj, "meta"), "rep"))) %>%
    dplyr::mutate(....id = as.character(1:(dplyr::n())))

  ## Extract all fit columns
  simpr_mods = obj %>%
    dplyr::select(tidyselect::one_of(c(attr(obj, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$....id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, function(x, ...) purrr::map_dfr(x, fn_map, ..., .id = "....id"),
                              ..., .id = "Source")

  ## Re-merge metaparameter columns to tidy output
  output = dplyr::right_join(simpr_meta, simpr_tidy, by = "....id")
  output$....id = NULL

  output
}
