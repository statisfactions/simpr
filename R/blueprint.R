#' Specify functions to generate variables
#'
#' Specify functions for computing each variable in the simulation.
#'
#' This is always the first command in the simulation process, to specify the
#' actual simulated variables.
#'
#' The \code{\dots} arguments use an efficient syntax to specify custom
#' functions needed for generating a simulation, based on the \code{purrr}
#' package.  When producing one variable, one can provide an expression such as
#' \code{blueprint(x = ~ 3 + runif(10))} instead of defining a custom function.
#'
#' If a formula is given as a named argument, the name is used for the name(s)
#' of the generated variables. For instance, if the argument \code{x} generates
#' a two-column matrix and \code{sep = "_"} (the default) the variables will be
#' named \code{x_1} and \code{x_2}. Double-sided formulas specify names for
#' multiple columns produced by the function, and can be specified using
#' \code{\link{cbind}(name1, name2, etc)}, similar to multivariate
#' specifications elsewhere in R, e.g. \code{cbind(x, y) ~ MASS::mvrnorm(5, c(0,
#' 0), Sigma = diag(2))}.
#'
#' @param .x a \code{simpr_spec} object (the output
#'   of \code{\link{meta}}), or NULL to
#'   create a new specification
#' @param ... \code{purrr}-style formula functions used for generating
#'   simulation variables.
#' @param sep Specify the separator for auto-generating names.  See
#'   \emph{Details}.
#' @return A \code{simpr_blueprint} object which contains the functions needed to
#'   generate the simulation; to be passed to \code{\link{meta}} for defining
#'   metaparameters or, if there are no metaparameters, directly to
#'   \code{\link{generate}} for generating the simulation.
#'
#'   Also useful is the fact that one can refer to variables in subsequent
#'   arguments.  So, one could define another variable \code{y} that depends on
#'   \code{x} very simply, e.g. \code{blueprint(x = ~ 3 + runif(10), y = ~ 2 *
#'   x)}.
#'
#'   Finally, one can also refer to metaparameters that are to be systematically
#'   varied in the simulation study.  See \code{\link{meta}} and the examples
#'   for more details.
#'
#' @examples
#' ## specify a variable and generate it in the simulation
#' single_var = blueprint(x = ~ 1 + rnorm(5)) %>%
#'   generate(1) # generate a single repetition of the simulation
#' single_var$sim[[1]] # peek at the simulation
#'
#' two_var = blueprint(x = ~ 1 + rnorm(5),
#'                     y = ~ x + 2) %>%
#'   generate(1)
#' two_var$sim[[1]]
#'
#' ## Generates x_01 through x_10
#' autonumber_var = blueprint(x = ~ MASS::mvrnorm(5, rep(0, 10), Sigma = diag(10))) %>%
#'   generate(1)
#' autonumber_var$sim[[1]]
#'
#' # alternatively, you could use a two-sided formula for names
#' multi_name = blueprint(cbind(x, y, z) ~ MASS::mvrnorm(5, rep(0, 3), Sigma = diag(3))) %>%
#'   generate(1)
#' multi_name$sim[[1]]
#'
#' # Simple example of setting a metaparameter
#' simple_meta = blueprint(x = ~ 1 + rnorm(n)) %>%
#'   meta(n = c(5, 10)) %>% # without this line you would get an error!
#'   generate(1)
#'
#'
#' simple_meta # has two rows now, one for each value of n
#' simple_meta$sim[[1]]
#' simple_meta$sim[[2]]
#'
#' @export
blueprint = function(.x = NULL, ..., sep = "_") {

  vars = list(...)

  if(!is.null(.x) && is.simpr_spec(.x)) {
    out = .x
  } else {
    out = new_simpr_spec()
    if(!is.null(.x)) {
      ## If not a simpr_spec, treat as an unnamed
      ## argument for the two-sided formula
      ## interface
      vars = c(.x, vars)
    }

  }



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
  out$blueprint = purrr::pmap(list(vars, names(vars), named_vars),
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

                                             x_out = x
                                             attr(x_out, "varnames") = n
                                             x_out

                                           }

                                         }

                                       }
                                     })


  # set attribute of "sep" for auto-numbering variables with multiple outputs
  out$variable_sep = sep

  out
}
