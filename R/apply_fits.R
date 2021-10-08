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
#' @inheritParams fit
#' @seealso \code{\link{tidy_fits}}, \code{\link{glance_fits}}
#' @export
apply_fits = function(obj, .f, ..., .progress = FALSE,
                      .options = furrr_options()) {
  UseMethod("apply_fits")
}


#' @export
apply_fits.simpr_spec = function(obj, .f, ..., .progress = FALSE,
                                 .options = furrr_options()) {
  mc = match.call()

  add_call(obj, mc, "apply_fits", replace_arg = "obj")
}

#' @export
apply_fits.simpr_tibble = function(obj, .f, ..., .progress = FALSE,
                                   .options = furrr_options()) {

  obj = as_tibble(obj, bare_tibble = TRUE)
  fn_map = function(...) dplyr::as_tibble(purrr::as_mapper(.f)(...))
  ## Create reference meta df for merging
  simpr_meta = obj %>%
    dplyr::select(tidyselect::one_of(c(".sim_id", attr(obj, "meta"), "rep")))

  ## Extract all fit columns
  simpr_mods = obj %>%
    dplyr::select(tidyselect::one_of(c(attr(obj, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$.sim_id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, function(x, ...)
    furrr::future_map_dfr(x, fn_map, ..., .id = ".sim_id",
                          .progress = .progress,
                          .options = .options),
                              ..., .id = "Source")
  simpr_tidy$.sim_id = as.integer(simpr_tidy$.sim_id)

  ## Re-merge metaparameter columns to tidy output
  output = dplyr::right_join(simpr_meta, simpr_tidy, by = ".sim_id")

  output
}
