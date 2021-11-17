#' Methods for simpr_spec class
#'
#' Accessor & display methods for simpr_spec class
#'
#' Class \code{simpr_spec} is created by
#' \code{\link{blueprint}} and/or
#' \code{\link{meta}} to specify the simulation
#' variables, which is produced by
#' \code{\link{generate}}.
#'
#' @param x a \code{simpr_spec} object
#' @param \dots ignored
#'
#' @rdname simpr_spec
#' @export
print.simpr_spec = function(x, ...) {
  print_cats = x[c("blueprint", "conditions", "meta_info", "include_calls")]
 purrr::imap(print_cats, function(y, ynm) {
   if(length(y) == 0 || (ynm == "meta_info" && length(y$indices) == 0))
     return()
   cat(ynm, "\n--------------------------\n", sep = "")
   if(ynm == "meta_info") {
     purrr::imap(y$lookup, function(z, znm) {
      cat(znm, ":\n", sep = "")
      print(z[[2]])
     })
   } else {
     print(y)
   }
   cat("\n", sep = "")
 })

}


new_simpr_spec = function() {
  x = list(conditions = tibble::tibble(),
           meta_info = list(indices = list(),
                            lookup = list()),
           blueprint = list(),
           variable_sep = character(),
           include_calls = list())

  class(x) = "simpr_spec"

  x
}

is.simpr_spec = function(x) {
 empty = new_simpr_spec()

 class(x) == "simpr_spec" &&
 is.list(x) &&
   all(names(x) == names(empty)) &&
   all(names(x$meta_info) == names(empty$meta_info))



}
