#' Specify functions to generate variables
#'
#' Creates a simpr class object that specifies formulae for computing each variable in the model.
#'
#' @return simpr class object
#'
#' @examples
#' variables(x1 = ~ 2 + rnorm(n),
#'     x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
#'     y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = s))
#'
#' @export
variables = function(..., sep = "_") {

  vars = list(...)

  if(length(vars) == 0)
    stop("No variables defined")

  ## Identify named arguments
  if(is.null(names(vars))) {
    named_vars = rep(FALSE, length(vars))
    names(vars) = paste0(".unnamed_",1:length(vars))
  } else {
    named_vars =  names(vars) != "" # empty names become "" when there are both named and unnamed args
    names(vars)[!named_vars] =  paste0(".unnamed_", names(vars)[!named_vars])
  }

  # Process formulas to extract and set varnames attribute
  out = list(variables = purrr::pmap(list(vars, names(vars), named_vars),
                                     function(x, n, named) {

                                       if(!rlang::is_formula(x))
                                         stop("Argument is not formula")
                                       else {
                                         ## Double-sided formula
                                         if(length(x) == 3) {
                                           if(named)
                                             warning("Two-sided formula given as named argument but will be ignored")

                                           ## Get names from left-hand side of formula
                                           attr(x, "varnames") = x[[2]][-1] %>% purrr::map_chr(deparse)

                                           ## delete left-hand side of formula and return right-handed formula
                                           x_out = x
                                           x_out[[2]] = NULL

                                           x_out
                                         } else {
                                           ## Single-sided formula
                                           if(length(x) == 2) {
                                             if(!named)
                                               stop("Right-hand formulas must be named.")

                                             # Get name from name of argument
                                             x_out = x
                                             attr(x_out, "varnames") = n

                                             x_out

                                           }

                                         }

                                       }
                                     }))


  # set attribute of "sep" for auto-numbering variables with multiple outputs
  attr(out$variables, "sep") = sep

  class(out) = "simpr"
  out
}
