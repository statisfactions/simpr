#' Specify metaparameters to vary in simulation
#'
#' Takes the output of \code{\link{blueprint}} (a
#' \code{simpr_blueprint} object) and defines the
#' metaparameters for simulation.
#'
#' This is the second step in the simulation
#' process, after specifying the simulated data
#' using \code{\link{blueprint}}.  The output of
#' \code{\link{meta}} is then passed to
#' \code{\link{produce}} to actually generate the
#' simulation.
#'
#' Metaparameters are named arguments that are
#' used in the simulation.  Usually, a
#' metaparameter is some kind of vector or list,
#' representing something that is to be
#' systematically varied as a part of the
#' simulation design. Any metaparameter would also
#' appear in the formulas of
#' \code{\link{blueprint}}, and thus the
#' simulation changes depending on the value of
#' the metaparameter.
#'
#' When creating the simulation, simulations for
#' all possible combinations of metaparameters are
#' generated, a fully crossed simulation design.
#'
#' When one of \code{\dots} is a list, a new
#' column is generated in the output to
#' \code{produce} to serve as the index of the
#' list.  This new column will be the name of the
#' list argument,  with the \code{suffix} argument
#' appended onto the end.  So if \code{Y = list(a
#' = 1:2, b = letters[2:3])}, and \code{suffix =
#' "_index"}, the default, a column named
#' \code{Y_index} would be added to the output of
#' \code{produce} with values \code{"a"} and
#' \code{"b"}.
#'
#' @param x a \code{simpr_blueprint} object (the output
#'   of \code{\link{blueprint}})
#' @param ... metaparameters: named arguments
#'   containing vectors or unidimensional lists of
#'   objects to be used in the simulation.
#' @param suffix name of suffix to append onto
#'   index column for list metaparameters,
#'   \code{"_index"} by default.  See
#'   \emph{Details}.
#' @return a \code{simpr_meta} object to pass onto
#'   \code{\link{produce}} for the simulation.
#'   This is also a tibble containing all
#'   simulation conditions that can be examined or
#'   altered directly (in case a fully crossed
#'   design is not desired).
#'
#' @examples
#' # Simple example of setting a metaparameter
#' simple_meta = blueprint(x = ~ 1 + rnorm(n)) %>%
#'   meta(n = c(5, 10)) %>%
#'   produce(1)
#'
#' simple_meta # $sim_cell has a 5-row tibble and a 10-row tibble
#'
#' multi_meta = blueprint(x = ~ mu + rnorm(n)) %>%
#'   meta(n = c(5, 10),
#'        mu = seq(-1, 1, length.out = 3)) %>%
#'   produce(1)
#'
#' multi_meta # generates simulations for all combos of n and mu
#'
#'
#' # meta can handle lists which can contain multiple matrices, etc.
#' meta_list_out = blueprint(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
#'   meta(n = c(10, 20, 30),
#'        S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
#'   produce(1)
#'
#' meta_list_out # generates S_index column
#'
#' @export
meta = function(x, ..., suffix = "_index") {
  if(!(is.character(suffix)) || length(suffix) != 1 || nchar(suffix) <= 0)
    stop("suffix must be a string with at least 1 character")

  meta = list(...)

  ## Create "index" element from names of list_elements
  list_elements = meta %>% purrr::map_lgl(is.list)

  ## Check list elements to see if dims work, assign index columns based on
  ## names of list
  if(any(list_elements)) {
    index_lookup = purrr::imap(meta[list_elements], function(x, n) {
      if(length(dim(x)) == 2) {
        stop("list arguments must be uni-dimensional")
      } else {
        if(is.null(names(x))) {
          index = 1:length(x)
        } else {
          index = names(x)
        }
      }

      lookup = tibble::tibble(index = index, value = x)
      names(lookup) = c(paste0(n, suffix), n)

      list(index = index,
           lookup = lookup)
    })


    names(index_lookup) = paste0(names(index_lookup), suffix)
    indices = purrr::map(index_lookup, "index")

    x$meta = list(indices = c(meta[!list_elements], purrr::map(index_lookup, "index")),
                  lookup =  purrr::map(index_lookup, "lookup"))

  } else {
    x$meta = list(indices = meta,
                  lookup = NULL)
  }

  specs = expand.grid(x$meta$indices, stringsAsFactors = FALSE) %>%
    tibble::as_tibble()

  class(specs) = c("simpr_meta", class(specs))

  attr(specs, "meta") = x$meta
  attr(specs, "variables") = x$variables

  specs
}

