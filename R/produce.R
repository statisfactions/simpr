#' Produce simulated data from specification
#'
#' Use specification from \code{\link{blueprint}}
#' or \code{\link{meta}} to produce simulated
#' data.
#'
#' This is the third step in the simulation
#' process: after specifying the variables and
#' metaparameters, \code{produce} is the workhorse
#' function that actually generates the simulated
#' datasets, one for each replication, for each
#' combination of metaparameters. You likely want
#' to use the output of \code{produce} to fit
#' model(s) with \code{\link{fit}}.
#'
#' Errors you get using this function usually have
#' to do with how you specified the simulation in
#' \code{\link{blueprint}} and \code{\link{meta}}.
#'
#' @param obj a \code{simpr_spec} object generated
#'   by \code{\link{meta}} or
#'   \code{\link{blueprint}}, containing the
#'   specifications of the simulation
#' @param reps number of replications to run (a
#'   whole number greater than 0)
#' @seealso \code{\link{blueprint}} and
#'   \code{\link{meta}} for examples of how these
#'   functions affect the output of \code{produce}
#' @return a \code{simpr_gen} object, which is a
#'   tibble with a row for each repetition (a
#'   total of \code{rep} repetitions) for each
#'   combination of metaparameters and some extra
#'   metadata used by \code{\link{fit}}.  The
#'   columns are \code{rep} for the repetition
#'   number, the names of the metaparameters, and
#'   a list-column \code{sim_cell} containing the
#'   dataset for each repetition and metaparameter
#'   combination.
#' @examples
#' meta_list_out = blueprint(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
#'   meta(n = c(10, 20, 30),
#'        S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
#'   produce(3)
#'
#'  ## View overall structure of the result
#'  meta_list_out
#'
#'  ## View an individual dataset of the resulting simulation
#'  meta_list_out$sim_cell[[1]]
#'
#'  ## Changing reps will change the number of replications and thus the number of
#'  ## rows in the output
#'  meta_list_2 = blueprint(x = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
#'   meta(n = c(10, 20, 30),
#'        S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
#'   produce(4)
#'
#'  meta_list_2
#'
#' @export
produce = function(obj, reps) {
  validate_reps(reps)

  specs = dplyr::left_join(data.frame(rep = 1:reps),
                           obj$conditions,
                           by = character())

  if(!is.null(obj$meta_info$lookup)) {
    ## If there are list elements, join cells representing those list-columns
    ## into specs
    specs = purrr::reduce2(obj$meta_info$lookup,
                           dplyr::inner_join,
                           .init = specs,
                           .y = names(obj$meta$lookup)) # the "by" argument to the join
  }

  create_sim_results(specs = specs, x = obj[c("meta_info",
                                            "blueprint",
                                            "variable_sep",
                                            "include_calls")])
}



generate_sim_cell = function(variables, ..., variable_sep, include_calls) {
  meta_values = list(...)
  eval_environment = rlang::as_environment(meta_values, parent = parent.frame())

  df = purrr::map_dfc(variables, function(y) {

    eval_fun = purrr::as_mapper(y)
    environment(eval_fun) <- eval_environment

    gen = eval_fun()

    varnames = attr(y, "varnames")

    if(is.null(ncol(gen))) {
      gen_df = tibble::as_tibble(gen, .name_repair = "minimal")
      names(gen_df) = varnames
      assign(varnames, gen, envir = eval_environment)

    } else if(length(dim(gen)) > 3) {
      stop("More than 2 dimensional output in blueprint() not supported")
    } else if(ncol(gen) == 0) {
      stop("variable function returns 0 columns")
    } else if(ncol(gen) == 1) {
      names(gen) = attr(y, "varnames")
      assign(varnames, gen[[1]], envir = eval_environment)
    } else if(ncol(gen) > 1) {
      gen_df = tibble::as_tibble(gen, .name_repair = "minimal")

      # rename gen_df
      ## if multiple varnames given via formula lhs, use those
      if(length(varnames) > 1) {
        names(gen_df) = varnames
      } else {
        # Otherwise, use auto-numbering
        names(gen_df) = sprintf(paste0("%s%s%0", nchar(trunc(ncol(gen))), ".0f"),
                                varnames,
                                variable_sep,
                                1:ncol(gen))
        ## assign names to the eval_environmnent
      }
      purrr::iwalk(gen_df, ~ assign(.y, .x, envir = eval_environment))


    }

    gen_df
  })

  df_full = data.frame(meta_values) %>% tibble::as_tibble() %>%
    dplyr::mutate(sim_cell = list(df))

  if(is.null(include_calls)) {
    return(df_full)
  } else {

    ## Add "simpr_produce" class
    class(df_full) = c("simpr_produce", class(df_full))

    df_eval = purrr::reduce(.x = include_calls, .f = eval_pipe, .init = df_full)

    return(df_eval)
    }
}

eval_pipe = function(lhs, rhs) {
  eval(call("%>%", lhs = lhs, rhs = rhs))
}

create_sim_results <- function(specs, x) {
  ## Create simulation results from specification

  ## define variable "." to satisfy R CMD Check
  . = "Defining ."

  ## Generate all replications
  sim_results = specs %>%
    tibble::as_tibble() %>%
    purrr::pmap(generate_sim_cell,
                variables = x$blueprint,
                include_calls = x$include_calls,
                variable_sep = x$variable_sep) %>%
    dplyr::bind_rows()

  ## Add some attributes to the tibble to track meta and variables
  attr(sim_results, "meta") = names(x$meta_info$indices)
  attr(sim_results, "variables") = purrr::map(x$variables, ~ attr(., "varnames")) %>% unlist

  ## Add "simpr_produce" class
  class(sim_results) = c("simpr_produce", class(sim_results))

  sim_results
}

validate_reps = function(reps) {
  ## Check reps argument is a whole number > 0
  if(length(reps) > 1) {
    reps = reps[1]
    warning("reps has length > 1, using only first element")
  }
  if(reps != round(reps)) {
    reps = round(reps)
    warning("reps not a whole number, rounding to nearest whole number")
  }
  if(reps < 1) {
    stop("reps should be at least 1")
  }
}
