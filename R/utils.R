#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom furrr future_options
#' @export
furrr::future_options

## Get the name of the simulation result column
get_sim_name = function(obj) {
  attr(obj, "sim_name")
}
