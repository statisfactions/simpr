#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

## Get the name of the simulation result column
get_sim_name = function(obj) {
  attr(obj, "sim_name")
}


# Run a given function on a simpr_mod object and tidy the output.
apply_fits = function(obj, FUN) {
 UseMethod("apply_fits")
}

apply_fits.simpr_spec = function(obj, FUN) {
  mc = match.call()

  add_call(obj, mc, "apply_fits", replace_arg = "obj")
}

apply_fits.simpr_tibble = function(obj, FUN) {
  ## Create reference meta df for merging
  simpr_meta = obj %>%
    dplyr::select(tidyselect::one_of(c(attr(obj, "meta"), "rep"))) %>%
    dplyr::mutate(....id = as.character(1:(dplyr::n())))

  ## Extract all fit columns
  simpr_mods = obj %>%
    dplyr::select(tidyselect::one_of(c(attr(obj, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$....id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, ~ purrr::map_dfr(., FUN, .id = "....id"), .id = "Source")

  ## Re-merge metaparameter columns to tidy output
  output = dplyr::right_join(simpr_meta, simpr_tidy, by = "....id")
  output$....id = NULL

  output
}
