#' Tidy the simulated model results output into tibble of components (broom analogue)
#'
#' Turn fitted model of simulated data (output of simpr::fit) into a tidy tibble of model components.
#'
#' @param simpr_mod simulated model results (output of simpr::fit)
#'
#' @return tidied model components of fitted simulated data
#'
#' @examples
#' calc_tidy(simpr_mod)
#'
#' @export
calc_tidy = function(simpr_mod) {
  ## Create reference meta df for merging
  simpr_meta = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "meta"), "rep"))) %>%
    dplyr::mutate(....id = as.character(1:(dplyr::n())))

  ## Extract all fit columns
  simpr_mods = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$....id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, ~ purrr::map_dfr(., broom::tidy, .id = "....id"), .id = "Source")

  ## Re-merge metaparameter columns to tidy output
  dplyr::right_join(simpr_meta, simpr_tidy, by = "....id") %>%
    dplyr::select(-....id)

}

#' Glance at the simulated model results output (broom analogue)
#'
#' Turn fitted model of simulated data (output of simpr::fit) into a tidy tibble of overall model statistics
#'
#' @param simpr_mod simulated model results (output of simpr::fit)
#'
#' @return tidied model summary of fitted simulated data
#'
#' @examples
#' calc_glance(simpr_mod)
#'
#' @export
calc_glance = function(simpr_mod) {
  simpr_meta = simpr_mod %>% dplyr::select(-mod)

  simpr_glance = simpr_mod %>%
    do(glance = broom::glance(.$mod)) %>%
    dplyr::bind_cols(simpr_meta, .) %>%
    tidyr::unnest()

}
