#' Methods for simpr_spec class
#'
#' Accessor & display methods for simpr_spec class
#'
#' Class \code{simpr_spec} is created by
#' \code{\link[=specify.formula]{specify}} and/or
#' \code{\link{define}} to specify the simulation
#' variables, which is produced by
#' \code{\link[=generate.simpr_spec]{generate}}.
#' The print method provides an overview of the
#' specification, including the conditions.
#'
#' @param x a \code{simpr_spec} object
#' @param \dots ignored
#' @return \code{print.simpr_spec} has no return value
#'   and is called for its side-effects.
#'   \code{new_simpr_spec} returns an empty
#'   \code{simpr_spec} object. \code{is.simpr_spec} returns
#'   a length-1 logical vector, \code{TRUE} or \code{FALSE},
#'   which indicates whether an object is a
#'   \code{simpr_spec}.
#' @examples
#' empty = new_simpr_spec()
#' print(empty)
#'
#' ## Easiest to create a simpr_spec with specify
#' simple_spec = specify(a = ~ rbinom(n, size, prob))
#' print(simple_spec)
#'
#' ## Adding on define adds all possible combinations
#' ## of conditions and more info in output.
#' defined_spec = specify(a = ~ rbinom(n, size, prob)) %>%
#'   define(n = c(10, 20),
#'          size = c(20, 40),
#'          prob = c(0.2, 0.4))
#' print(defined_spec)
#'
#' @rdname simpr_spec
#' @export
print.simpr_spec = function(x, ...) {
  print_cats = x[c("specify", "conditions", "meta_info", "include_calls")]
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

#' @export
#' @rdname simpr_spec
new_simpr_spec = function() {
  x = list(conditions = tibble::tibble(),
           meta_info = list(indices = list(),
                            lookup = list()),
           specify = list(),
           variable_sep = character(),
           include_calls = list(),
           .use_names = logical())

  class(x) = "simpr_spec"

  x
}

#' @export
#' @rdname simpr_spec
is.simpr_spec = function(x) {
 empty = new_simpr_spec()

 class(x) == "simpr_spec" &&
 is.list(x) &&
   all(names(x) == names(empty)) &&
   all(names(x$meta_info) == names(empty$meta_info))
}
