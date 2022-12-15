#' Run a given function or formula expression on a
#' simpr_mod object and tidy the output.
#'
#' This function applies a given function to all
#' fit objects and returns the result in a tidy
#' tibble.  Any function or \code{purrr}-style
#' lambda function can be used.
#'
#'
#' @param obj a \code{simpr_tibble} with
#'   repetition number, metaparameters, simulated
#'   data, and fitted models, from
#'   \code{\link[=fit.simpr_tibble]{fit}}
#' @param .f A function or \code{purrr}-style
#'   lambda function (see
#'   \code{\link[purrr]{as_mapper}}) used for
#'   computing on the fit object
#' @param \dots Additional arguments to \code{.f}.
#' @inheritParams fit.simpr_tibble
#' @return A \code{tibble} with columns \code{.sim_id},
#'   \code{rep}, \code{Source} (which contains the name of the
#'   fit column), any metaparameters from
#'   \code{\link{define}}, and additional columns
#'   containing the results of \code{.f} applied to each fit
#'   object.
#' @examples
#' set.seed(100)
#' logit_fit = specify(a = ~ sample(0:1, size = n, replace = TRUE),
#'         b = ~ a + rlnorm(n)) %>%
#'   define(n = c(40, 50)) %>%
#'   generate(1) %>%
#'   fit(logit = ~ glm(a ~ b, family = "binomial"))
#'
#' logit_fit %>%
#'   apply_fits(broom::augment)
#'
#' ## Arguments to the function can be passed in ...
#' logit_fit %>%
#'   apply_fits(broom::augment, se_fit = TRUE)
#'
#' ## Equivalent to tidy_fits()
#' logit_fit %>%
#'   apply_fits(broom::tidy)
#'
#' ## Using a purrr-style lambda function
#' logit_fit %>%
#'   apply_fits(~ summary(.)$cov.scaled)
#'
#' @seealso \code{\link{tidy_fits}},
#'   \code{\link{glance_fits}}
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

#' @importFrom rlang .data
#' @export
apply_fits.simpr_tibble = function(obj, .f, ..., .progress = FALSE,
                                   .options = furrr_options()) {

  obj = tibble::as_tibble(as.data.frame(obj)) ## strip attributes
  fn_map = function(...) tibble::as_tibble(purrr::as_mapper(.f)(...))
  ## Create reference meta df for merging
  simpr_meta_raw = obj %>%
    dplyr::select(tidyselect::one_of(c(".sim_id", attr(obj, "meta"), "rep")))

  simpr_meta = simpr_meta_raw

  if(".sim_error" %in% names(obj))
    simpr_meta$.sim_error = obj$.sim_error


  if(any(grepl("^\\.fit_error", names(obj)))) {
    fit_errors = obj %>%
      dplyr::select(".sim_id", tidyselect::starts_with(".fit_error_")) %>%
      tidyr::pivot_longer(-".sim_id", names_to = "Source", values_to = ".fit_error") %>%
      dplyr::mutate(Source = sub("^^\\.fit_error_", "", .data$Source))

    simpr_meta = dplyr::left_join(simpr_meta, fit_errors, by = c(".sim_id"))

  }


  ## Extract all fit columns
  fit_names = c(attr(obj, "fits"))

  simpr_mods = obj %>%
    dplyr::select(tidyselect::one_of(fit_names)) %>%
    purrr::map(purrr::set_names, nm = simpr_meta_raw$.sim_id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, function(x, ...)
    furrr::future_map_dfr(x, fn_map, ..., .id = ".sim_id",
                          .progress = .progress,
                          .options = .options),
                              ..., .id = "Source")
  simpr_tidy$.sim_id = as.integer(simpr_tidy$.sim_id)

  ## Re-merge metaparameter columns to tidy output
  if("Source" %in% names(simpr_meta))
    output = dplyr::left_join(simpr_meta, simpr_tidy, by = c(".sim_id", "Source"))
  else
    output = dplyr::left_join(simpr_meta, simpr_tidy, by = ".sim_id")

  output
}
