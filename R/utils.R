#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`


# Run a given function on a simpr_mod object and tidy the output.
run_on_fit = function(simpr_mod, FUN) {
  ## Create reference meta df for merging
  simpr_meta = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "meta"), "rep"))) %>%
    dplyr::mutate(....id = as.character(1:(dplyr::n())))

  ## Extract all fit columns
  simpr_mods = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$....id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, ~ purrr::map_dfr(., FUN, .id = "....id"), .id = "Source")

  ## Re-merge metaparameter columns to tidy output
  output = dplyr::right_join(simpr_meta, simpr_tidy, by = "....id")
  output$....id = NULL

  output
}
