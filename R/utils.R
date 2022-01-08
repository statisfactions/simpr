#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom furrr furrr_options
#' @export
furrr::furrr_options

## Get the name of the simulation result column
get_sim_name = function(obj) {
  attr(obj, ".sim_name")
}

## Get/set the total number of simulations
get_sim_total = function(obj) {
  attr(obj, "sim_total")
}
