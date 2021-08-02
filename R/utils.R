#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

## Get the name of the simulation result column
get_sim_name = function(obj) {
  attr(obj, "sim_name")
}
