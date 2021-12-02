#' Specify data-generating mechanisms
#'
#' Specify the data-generating mechanisms for the
#' simulation using purrr-style lambda functions.
#'
#' This is always the first command in the
#' simulation process, to specify the actual
#' simulated variables.
#'
#' The \code{\dots} arguments use an efficient
#' syntax to specify custom functions needed for
#' generating a simulation, based on the
#' \code{purrr} package.  When producing one
#' variable, one can provide an expression such as
#' \code{specify(x = ~ 3 + runif(10))} instead of
#' defining a custom function.
#'
#' The results of these functions are brought
#' together into a single tibble, so all functions
#' should produce the same number of rows.
#' However, a function can certainly produce
#' multiple columns as well.
#'
#' @section Column naming:
#'
#'   Because functions can produce different
#'   numbers of columns, some decisions need to be
#'   made regarding the names of those columns. If
#'   a provided lambda function produces a single
#'   column, the name given to the argument
#'   becomes the name of the column.  If the
#'   function already produces column names, then
#'   the output will use these names if
#'   \code{use_names = TRUE}, the default.
#'   Otherwise, simpr uses the argument name as a
#'   base and auto-numbers the columns. For
#'   instance, if the argument \code{x} generates
#'   a two-column matrix and \code{sep = "_"} (the
#'   default) the columns will be named
#'   \code{x_1}and \code{x_2}.
#'
#'   Custom names can also be directly provided by
#'   provide a double-sided formulas to provide
#'   names for multiple columns produced by the
#'   function. Double-sided formulas should not be
#'   named, since the left-hand side is already
#'   providing names. Multiple names can be
#'   provided on the left-hand side are provided
#'   using \code{cbind}, similar to multivariate
#'   specifications elsewhere in R, e.g.
#'   \code{specify(cbind(x, y) ~ MASS::mvrnorm(5,
#'   c(0, 0), Sigma = diag(2)))}.
#'
#' @param .x a \code{simpr_spec} object (the
#'   output of \code{\link{define}}), or NULL to
#'   create a new specification
#' @param ... \code{purrr}-style formula functions
#'   used for generating simulation variables.
#' @param sep Specify the separator for
#'   auto-generating names.  See \emph{Column
#'   naming}.
#' @param use_names Whether to use names generated
#'   by the lambda function (TRUE, the default), or to
#'   overwrite them with supplied names.
#' @return A \code{simpr_specify} object which
#'   contains the functions needed to generate the
#'   simulation; to be passed to
#'   \code{\link{define}} for defining
#'   metaparameters or, if there are no
#'   metaparameters, directly to
#'   \code{\link{generate}} for generating the
#'   simulation.
#'
#'   Also useful is the fact that one can refer to
#'   variables in subsequent arguments.  So, one
#'   could define another variable \code{y} that
#'   depends on \code{x} very simply, e.g.
#'   \code{specify(x = ~ 3 + runif(10), y = ~ 2 *
#'   x)}.
#'
#'   Finally, one can also refer to metaparameters
#'   that are to be systematically varied in the
#'   simulation study.  See \code{\link{define}}
#'   and the examples for more details.
#'
#' @examples
#' ## specify a variable and generate it in the simulation
#' single_var = specify(x = ~ 1 + rnorm(5)) %>%
#'   generate(1) # generate a single repetition of the simulation
#' single_var$sim[[1]] # peek at the simulation
#'
#' two_var = specify(x = ~ 1 + rnorm(5),
#'                     y = ~ x + 2) %>%
#'   generate(1)
#' two_var$sim[[1]]
#'
#' ## Generates x_01 through x_10
#' autonumber_var = specify(x = ~ MASS::mvrnorm(5, rep(0, 10), Sigma = diag(10))) %>%
#'   generate(1)
#' autonumber_var$sim[[1]]
#'
#' # alternatively, you could use a two-sided formula for names
#' multi_name = specify(cbind(x, y, z) ~ MASS::mvrnorm(5, rep(0, 3), Sigma = diag(3))) %>%
#'   generate(1)
#' multi_name$sim[[1]]
#'
#' # Simple example of setting a metaparameter
#' simple_meta = specify(x = ~ 1 + rnorm(n)) %>%
#'   define(n = c(5, 10)) %>% # without this line you would get an error!
#'   generate(1)
#'
#'
#' simple_meta # has two rows now, one for each value of n
#' simple_meta$sim[[1]]
#' simple_meta$sim[[2]]
#'
#' @export
specify = function(.x = NULL, ..., use_names = TRUE, sep = "_") {

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
  out$specify = purrr::pmap(list(vars, names(vars), named_vars),
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


  # set attributes of "use_names" and "sep" for auto-numbering variables with multiple outputs
  out$variable_sep = sep
  out$use_names = use_names

  out
}
