#' Generate simulated data from specification
#'
#' Use specification from
#' \code{\link[=specify.formula]{specify}} or
#' \code{\link{define}} to produce simulated data.
#'
#' This is the third step in the simulation
#' process: after specifying the population model
#' and defining the metaparameters, if any,
#' \code{generate} is the workhorse function that
#' actually generates the simulated datasets, one
#' for each replication and combination of
#' metaparameters. You likely want to use the
#' output of \code{generate} to fit model(s) with
#' \code{\link[=fit.simpr_tibble]{fit}}.
#'
#' Errors you get using this function usually have
#' to do with how you specified the simulation in
#' \code{\link[=specify.formula]{specify}} and
#' \code{\link{define}}.
#'
#' @param x a \code{simpr_spec} object generated
#'   by \code{\link{define}} or
#'   \code{\link[=specify.formula]{specify}},
#'   containing the specifications of the
#'   simulation
#' @param .reps number of replications to run (a
#'   whole number greater than 0)
#' @param \dots filtering criteria for which rows
#'   to simulate, passed to
#'   \code{\link[dplyr]{filter}}.  This is useful
#'   for reproducing just a few selected rows of a
#'   simulation without needing to redo the entire
#'   simulation, see \code{vignette("Reproducing
#'   simulations")},
#' @param .sim_name name of the list-column to be
#'   created, containing simulation results.
#'   Default is \code{"sim"}
#' @param .quiet Should simulation errors be
#'   broadcast to the user as they occur?
#' @param .warn_on_error Should there be a warning
#'   when simulation errors occur? See
#'   \code{vignette("Managing simulation
#'   errors")}.
#' @param .stop_on_error Should the simulation
#'   stop immediately when simulation errors
#'   occur?
#' @param .debug Run simulation in debug mode,
#'   allowing objects, etc. to be explored for
#'   each generated variable specification.
#' @param .progress	A logical, for whether or not
#'   to print a progress bar for multiprocess,
#'   multisession, and multicore plans.
#' @param .options The \code{future} specific
#'   options to use with the workers when using
#'   futures. This must be the result from a call
#'   to
#'   \code{\link[furrr:furrr_options]{furrr_options(seed
#'    = TRUE)}}.
#' @seealso
#'   \code{\link[=specify.formula]{specify}} and
#'   \code{\link{define}} for examples of how
#'   these functions affect the output of
#'   \code{generate}. See
#'   \code{vignette("Optimization")} and the
#'   \code{furrr} website for more information on
#'   working with futures:
#'   \url{https://furrr.futureverse.org/}
#' @return a \code{\link{simpr_sims}} object,
#'   which is a tibble with a row for each
#'   repetition (a total of \code{rep}
#'   repetitions) for each combination of
#'   metaparameters and some extra metadata used
#'   by \code{\link[=fit.simpr_tibble]{fit}}.  The
#'   columns are \code{rep} for the repetition
#'   number, the names of the metaparameters, and
#'   a list-column (named by the argument
#'   \code{sim_name}) containing the dataset for
#'   each repetition and metaparameter
#'   combination. \code{simpr_sims} objects can be
#'   manipulated elementwise by \code{dplyr} and
#'   \code{tidyr} verbs: the command is applied to
#'   each element of the simulation list-column.
#' @examples
#' meta_list_out = specify(a = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
#'   define(n = c(10, 20, 30),
#'        S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
#'   generate(3)
#'
#'  ## View overall structure of the result and a single simulation output
#'  meta_list_out
#'
#'  ## Changing .reps will change the number of replications and thus the number of
#'  ## rows in the output
#'  meta_list_2 = specify(a = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
#'   define(n = c(10, 20, 30),
#'        S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
#'   generate(4)
#'
#'  meta_list_2
#'
#'  ## Fitting, tidying functions can be included in this step by running those functions and then
#'  ## generate.  This can save computation time when doing large
#'  ## simulations, especially with parallel processing
#'  meta_list_generate_after = specify(a = ~ MASS::mvrnorm(n, rep(0, 2), Sigma = S)) %>%
#'   define(n = c(10, 20, 30),
#'        S = list(independent = diag(2), correlated = diag(2) + 2)) %>%
#'   fit(lm = ~ lm(a_2 ~ a_1, data = .)) %>%
#'   tidy_fits %>%
#'   generate(4)
#'
#'   meta_list_generate_after
#' @export
generate.simpr_spec = function(x, .reps, ..., .sim_name = "sim",
                    .quiet = TRUE, .warn_on_error = TRUE,
                    .stop_on_error = FALSE,
                    .debug = FALSE,
                    .progress = FALSE,
                    .options = furrr_options(seed = TRUE)) {

  validate_reps(.reps)

  ## Create specs from conditions, .reps; add unique id as well.
  specs = dplyr::left_join(data.frame(rep = 1:.reps),
                           x$conditions,
                           by = character()) %>%
    dplyr::mutate(.sim_id = 1:(dplyr::n())) %>%
    dplyr::relocate(".sim_id") %>%
    dplyr::relocate(rep, .after = dplyr::everything())

  specs_filter = dplyr::filter(specs, ...)

  excluded_sim_ids = specs$.sim_id[!(specs$.sim_id %in% specs_filter$.sim_id)]

  if(!is.null(x$meta_info$lookup)) {
    ## If there are list elements, join cells representing those list-columns
    ## into specs
    specs = purrr::reduce2(x$meta_info$lookup,
                           dplyr::inner_join,
                           .init = specs,
                           .y = names(x$meta$lookup)) # the "by" argument to the join
  }

  create_sim_results(specs = specs,
                     x = x[c("meta_info",
                               "specify",
                               "variable_sep",
                               ".use_names",
                               "include_calls")],
                     .sim_name = .sim_name,
                     .quiet = .quiet,
                     .debug = .debug,
                     .warn_on_error = .warn_on_error,
                     .stop_on_error = .stop_on_error,
                     .progress = .progress,
                     .options = .options,
                     excluded_sim_ids = excluded_sim_ids)
}


generate_sim = function(y, eval_environment, variable_sep, .use_names, .debug, .stop_on_error) {
  eval_fun = purrr::as_mapper(y)
  environment(eval_fun) <- eval_environment

  if(.debug)
    debug(eval_fun)

  gen = eval_fun()



  varnames = attr(y, "varnames")

  if(is.null(ncol(gen))) {
    gen_df = tibble::as_tibble(gen, .name_repair = "minimal")
    names(gen_df) = varnames
    assign(varnames, gen, envir = eval_environment)

  } else if(length(dim(gen)) > 3) {
    stop("More than 2 dimensional output in specify() not supported")
  } else if(ncol(gen) == 0) {
    stop("variable function returns 0 columns")
  } else if(ncol(gen) >= 1) {
    if(.use_names && !is.null(colnames(gen))) {
      varnames = colnames(gen)
    }

    if(ncol(gen) == 1) {
      names(gen) = attr(y, "varnames")
      assign(varnames, gen[[1]], envir = eval_environment)
    } else {
      gen_df = tibble::as_tibble(gen, .name_repair = "minimal")

      #rename gen_df # if multiple varnames given
      #via formula lhs or from the dataset itself,
      #use those
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
  }

  gen_df
}

generate_row = function(variables, ..., variable_sep, .use_names,
                             include_calls, meta_indices, .sim_name, .quiet,
                        .debug, .stop_on_error,
                        excluded_sim_ids) {

  meta_values = list(...)

  ## Exclude and don't calculate if this row is filtered out
  if(meta_values$.sim_id %in% excluded_sim_ids)
    return(NULL)

  eval_environment = rlang::as_environment(meta_values, parent = parent.frame())

  if(.stop_on_error)
    sim_list = purrr::map_dfc(variables, generate_sim,
                              eval_environment = eval_environment,
                              variable_sep = variable_sep,
                              .debug = .debug,
                              .use_names = .use_names)
  else
    sim_list = purrr::safely(purrr::map_dfc, otherwise = NULL, quiet = .quiet)(variables, generate_sim,
                                                                              eval_environment = eval_environment,
                                                                              variable_sep = variable_sep,
                                                                              .debug = .debug,
                                                                              .use_names = .use_names)

  ## Create a 1-row tibble with meta values and the simulation cell
  df_full = purrr::map(meta_values, ~ if(length(.) == 1 && purrr::is_vector(.)) return(.) else return(list(.))) %>%
    tibble::as_tibble(.rows = 1)

  df_full[[.sim_name]] = list(sim_list$result)

  if(!is.null(sim_list$error))
    df_full[[".sim_error"]] = as.character(sim_list$error)

  if(is.null(include_calls)) {
    return(df_full)
  } else {

    ## Add "simpr_tibble" class
    class(df_full) = c("simpr_tibble", class(df_full))

    ## identify which variables are meta variables
    attr(df_full, "meta") = meta_indices
    attr(df_full, ".sim_name") = .sim_name

    df_eval = purrr::reduce(.x = include_calls, .f = eval_pipe, .init = df_full) %>%
      tibble::as_tibble()

    return(df_eval)
    }
}

eval_pipe = function(lhs, rhs) {
  ## Set future options here since not evaluated
  ## by include(), and future options are ignored
  ## anyway
  .options = furrr_options(seed = TRUE)
  .progress = FALSE
  eval(call("%>%", lhs = lhs, rhs = rhs))
}

create_sim_results <- function(specs, x, .sim_name, .quiet, .warn_on_error, .progress, .options,
                               .debug, .stop_on_error,
                               excluded_sim_ids) {
  ## Create simulation results from specification

  ## Generate all replications
  sim_results = specs %>%
    tibble::as_tibble() %>%
    furrr::future_pmap(generate_row,
                variables = x$specify,
                include_calls = x$include_calls,
                variable_sep = x$variable_sep,
                .use_names = x$.use_names,
                meta_indices = names(x$meta_info$indices),
                .debug = .debug,
                .stop_on_error = .stop_on_error,
                .sim_name = .sim_name,
                .quiet = .quiet,
                .progress = .progress,
                .options = .options,
                excluded_sim_ids = excluded_sim_ids) %>%
    dplyr::bind_rows()

  ## Give warning if errors occured
  if(.warn_on_error && ".sim_error" %in% names(sim_results))
    warning("Simulation produced errors.  See column '.sim_error'.")

  ## Add some attributes to the tibble to track meta and variables
  attr(sim_results, "meta") = names(x$meta_info$indices)
  attr(sim_results, "variables") = purrr::map(x$variables, ~ attr(., "varnames")) %>% unlist
  attr(sim_results, ".sim_name") = .sim_name
  attr(sim_results, "sim_total") = max(specs$.sim_id)

  ## Add "simpr_tibble" class if there is still a sim column
  if(get_sim_name(sim_results) %in% names(sim_results))
    class(sim_results) = c("simpr_tibble", class(sim_results))

  sim_results
}

validate_reps = function(.reps) {
  ## Check .reps argument is a whole number > 0
  if(length(.reps) > 1) {
    .reps = .reps[1]
    warning(".reps has length > 1, using only first element")
  }
  if(.reps != round(.reps)) {
    .reps = round(.reps)
    warning(".reps not a whole number, rounding to nearest whole number")
  }
  if(.reps < 1) {
    stop(".reps should be at least 1")
  }
}

#' @importFrom generics generate
#' @export
generics::generate
