#' Specify metaparameters to vary in simulation
#'
#' Takes the output of \code{\link{variables}} (a \code{simpr_spec} object) and
#' defines the metaparameters for simulation.
#'
#' Metaparameters are named arguments that can be
#'
#' @param x simpr class object (e.g., output of variables())
#' @param ... metaparameters
#' @param suffix name of suffix to append onto index for list metaparameters
#' @return simpr class object
#'
#' @examples
#' variables(x1 = ~ 2 + rnorm(n),
#'     x2 = ~ 3 + 2*x1 + rnorm(n, 0, sd = 0.5),
#'     y = ~ 5 + b1*x1 + b2*x2 + g1*x1*x2 + rnorm(n, 0, sd = s)) \%>\%
#'         meta(n = seq(100, 300, by = 20),
#'              b1 = 1,
#'              b2 = 1,
#'              g1 = seq(-1, 1, by = 0.5),
#'              s = seq(0.2, 50, length.out = 6))
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

      lookup = tibble(index = index, value = x)
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

  x
}

