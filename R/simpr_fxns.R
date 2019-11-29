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

#' Specify metaparameters to vary in simulation
#'
#' Takes a simpr class object (e.g., from variables()) and defines the metaparameters for simulation.
#'
#' @param x simpr class object (e.g., output of variables())
#' @param \dots metaparameters
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

#' Generate simulated data from specified simpr model (from simpr::variables \%>\% simpr::meta())
#'
#' Takes a simpr class object with metaparameters (e.g., output of simpr::variables() \%>\% simpr::meta()) and generates simulated data to be fit with a statistical model (e.g., lm().
#'
#' @param simpr simpr class object with metaparamters (e.g., output of simpr::variables() \%>\% simpr::meta())
#' @param reps number of replications to run (integer)
#'
#' @examples
#' gen(simpr, reps = 1000)
#'
#' @return Simulated data
#'
#' @export
gen = function(simpr, reps) {
  # Create data frame representing all possible values of meta parameter indices
  specs = expand.grid(c(list(rep = 1:reps), simpr$meta$indices))


  if(!is.null(simpr$meta$lookup)) {
    ## If there are list elements, join cells representing those list-columns
    ## into specs
   specs = purrr::reduce2(simpr$meta$lookup,
                  inner_join,
                  .init = specs,
                  .y = names(simpr$meta$lookup)) # the "by" argument to the join
  }

  ## Generate all replications
  sim_results = specs %>%
    dplyr::group_by_at(.vars = c("rep", names(simpr$meta$indices))) %>%
    dplyr::do(sim_cell = purrr::pmap(., function(...) {
      eval_environment = rlang::as_environment(list(...), parent = parent.frame())

      df = purrr::map_dfc(simpr$variables, function(x) {

        eval_fun = purrr::as_mapper(x)
        environment(eval_fun) <- eval_environment

        gen = eval_fun() %>%
          unlist

        varnames = attr(x, "varnames")

        if(is.null(ncol(gen))) {
          gen_df = tibble::as_tibble(gen, .name_repair = "minimal")
          names(gen_df) = varnames
          assign(varnames, gen, envir = eval_environment)

        } else if(length(dim(gen)) > 3) {
            stop("More than 2 dimensional output in variables() not supported")
          } else if(ncol(gen) == 0) {
            stop("variable function returns 0 columns")
          } else if(ncol(gen) == 1) {
            names(gen) = attr(x, "varnames")
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
                                  attr(simpr$variables, "sep"),
                                  1:ncol(gen))
            ## assign names to the eval_environmnent
            }
            purrr::iwalk(gen_df, ~ assign(.y, .x, envir = eval_environment))


          }

        gen_df
      })


      df

    })) %>% tidyr::unnest(cols = c(sim_cell))

  ## Add some attributes to the tibble to track meta and variables
  attr(sim_results, "meta") = names(simpr$meta$indices)
  attr(sim_results, "variables") = purrr::map(simpr$variables, ~ attr(., "varnames")) %>% unlist

  sim_results
}


#' Fit the simulated data (from simpr::gen)
#'
#' Takes simulated data (output of simpr::gen) and fits to the specified statistical model.
#'
#' @param simpr_gen simulated data (output of simpr::gen)
#'
#' @return fitted model object of simulated data
#'
#' @examples
#' fit(simpr_gen, lm = ~lm(y ~ x1*x2, data = simpr_gen))
#'
#' @export
fit = function(simpr_gen, ...) {

  fit_functions = list(...)

  simpr_mod = simpr_gen

  for(i in names(fit_functions))
    simpr_mod[[i]] = purrr::map(simpr_mod$sim_cell, fit_functions[[i]])

  attr(simpr_mod, "meta") = attr(simpr_gen, "meta")
  attr(simpr_mod, "variables") = attr(simpr_gen, "variables")
  attr(simpr_mod, "fits") = c(attr(simpr_gen, "fits"), names(fit_functions))

  simpr_mod
}

# fit_lm = function(simpr_gen, ...) {
#   ## ... = arguments to lm
#   simpr_mod = simpr_gen %>%
#     group_by_at(c(attr(simpr_gen, "meta"), "rep")) %>%
#     do(mod = lm(data = ., ...))
#
#   attr(simpr_mod, "meta") = attr(simpr_gen, "meta")
#   attr(simpr_mod, "variables") = attr(simpr_gen, "variables")
#
#   simpr_mod
# }

#' Tidy the simulated model results output into tibble of components (broom analogue)
#'
#' Turn fitted model of simulated data (output of simpr::fit) into a tidy tibble of model components.
#'
#' @param simpr_mod simulated model results (output of simpr::fit)
#'
#' @return tidied model components of fitted simulated data
#'
#' @examples
#' calc_tidy(simpr_mod)
#'
#' @export
calc_tidy = function(simpr_mod) {
  ## Create reference meta df for merging
  simpr_meta = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "meta"), "rep"))) %>%
    dplyr::mutate(....id = as.character(1:(dplyr::n())))

  ## Extract all fit columns
  simpr_mods = simpr_mod %>%
    dplyr::select(tidyselect::one_of(c(attr(simpr_mod, "fits")))) %>%
    purrr::map(purrr::set_names, nm = simpr_meta$....id)

  ## For each fit column (identified as "source"), run tidy on each element of that column
  simpr_tidy = purrr::map_dfr(simpr_mods, ~ purrr::map_dfr(., broom::tidy, .id = "....id"), .id = "Source")

  ## Re-merge metaparameter columns to tidy output
  dplyr::right_join(simpr_meta, simpr_tidy, by = "....id") %>%
    dplyr::select(-....id)

}

#' Glance at the simulated model results output (broom analogue)
#'
#' Turn fitted model of simulated data (output of simpr::fit) into a tidy tibble of overall model statistics
#'
#' @param simpr_mod simulated model results (output of simpr::fit)
#'
#' @return tidied model summary of fitted simulated data
#'
#' @examples
#' calc_glance(simpr_mod)
#'
#' @export
calc_glance = function(simpr_mod) {
  simpr_meta = simpr_mod %>% dplyr::select(-mod)

  simpr_glance = simpr_mod %>%
    do(glance = broom::glance(.$mod)) %>%
    dplyr::bind_cols(simpr_meta, .) %>%
    tidyr::unnest()

}
