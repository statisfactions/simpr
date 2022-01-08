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
#' \code{specify(a = ~ 3 + runif(10))} instead of
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
#'   \code{.use_names = TRUE}, the default.
#'   Otherwise, simpr uses the argument name as a
#'   base and auto-numbers the columns. For
#'   instance, if the argument \code{a} generates
#'   a two-column matrix and \code{.sep = "_"} (the
#'   default) the columns will be named
#'   \code{a_1}and \code{a_2}.
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
#'   \code{specify(cbind(a, b) ~ MASS::mvrnorm(5,
#'   c(0, 0), Sigma = diag(2)))}.
#'
#' @param x leave this argument blank (NULL); this
#'   argument is a placeholder and can be skipped.
#' @param ... named \code{purrr}-style formula
#'   functions used for generating simulation
#'   variables. \code{x} is not recommended as a
#'   name, since it is a formal argument and will
#'   be automatically assumed to be the first
#'   variable (a message will be displayed if \code{x} is used).
#' @param .sep Specify the separator for
#'   auto-generating names.  See \emph{Column
#'   naming}.
#' @param .use_names Whether to use names generated
#'   by the lambda function (TRUE, the default),
#'   or to overwrite them with supplied names.
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
#'   could define another variable \code{b} that
#'   depends on \code{a} very simply, e.g.
#'   \code{specify(a = ~ 3 + runif(10), b = ~ 2 *
#'   x)}.
#'
#'   Finally, one can also refer to metaparameters
#'   that are to be systematically varied in the
#'   simulation study.  See \code{\link{define}}
#'   and the examples for more details.
#'
#' @examples
#' ## specify a variable and generate it in the simulation
#' single_var = specify(a = ~ 1 + rnorm(5)) %>%
#'   generate(1) # generate a single repetition of the simulation
#' single_var$sim[[1]] # peek at the simulation
#'
#' two_var = specify(a = ~ 1 + rnorm(5),
#'                     b = ~ x + 2) %>%
#'   generate(1)
#' two_var$sim[[1]]
#'
#' ## Generates a_01 through a_10
#' autonumber_var = specify(a = ~ MASS::mvrnorm(5, rep(0, 10), Sigma = diag(10))) %>%
#'   generate(1)
#' autonumber_var$sim[[1]]
#'
#' # alternatively, you could use a two-sided formula for names
#' multi_name = specify(cbind(a, b, c) ~ MASS::mvrnorm(5, rep(0, 3), Sigma = diag(3))) %>%
#'   generate(1)
#' multi_name$sim[[1]]
#'
#' # Simple example of setting a metaparameter
#' simple_meta = specify(a = ~ 1 + rnorm(n)) %>%
#'   define(n = c(5, 10)) %>% # without this line you would get an error!
#'   generate(1)
#'
#'
#' simple_meta # has two rows now, one for each value of n
#' simple_meta$sim[[1]]
#' simple_meta$sim[[2]]
#'
#' @export
specify.formula = function(x = NULL, ..., .use_names = TRUE, .sep = "_") {
  ## Method for creating a new simpr_spec object,
  ## which means that first argument must be a formula

  ## Note that this method uses S3 dispatch in a
  ## tricky way; usually the user will not
  ## actually specify anything called "x", but
  ## still this method is dispatched.  This is
  ## written to work more gracefully with the way
  ## that generics::specify is written

  vars = list(...)

  ## Normally x is ignored, but if the user does
  ## provide a variable called "x" we need to
  ## include that as well
  if(!is.null(x)) {
    message("Formula specification for 'x' detected. ",
    "Assuming 'x' is the first formula.\n\n",
    "To hide this message, or to avoid moving this formula first, ",
    "use a different variable name.")

    vars = c(list(x = x), vars)
  }

  add_specification(new_simpr_spec(),
                    varlist = vars,
                    .sep = .sep,
                    .use_names = .use_names)

}

add_specification = function(spec, varlist, .sep, .use_names) {

  if(length(varlist) == 0)
    stop("No variables defined")

  if(!all(purrr::map_lgl(varlist, purrr::is_formula))) {
    stop("All specifications should be purr-style formula functions")
  }

  ## Identify named arguments
  if(is.null(names(varlist))) {
    named_varlist = rep(FALSE, length(varlist))
    names(varlist) = paste0(".unnamed_",1:length(varlist))
  } else {
    named_varlist =  names(varlist) != "" # empty names become "" when there are both named and unnamed args
    names(varlist)[!named_varlist] =  paste0(".unnamed_", names(varlist)[!named_varlist])
  }

  # Process formulas to extract and set varnames attribute
  spec$specify = purrr::pmap(list(varlist, names(varlist), named_varlist),
                            function(x, n, named) {

                              if(!rlang::is_formula(x))
                                stop("Argument is not formula")
                              else {
                                ## Double-sided formula
                                if(length(x) == 3) {
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


  # set attributes of ".use_names" and ".sep" for auto-numbering variables with multiple outputs
  spec$variable_sep = .sep
  spec$.use_names = .use_names

  spec
}

#' @importFrom generics specify
#' @export
generics::specify






