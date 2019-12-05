#' Generate simulated data from specified simpr model (from simpr::variables \%>\% simpr::meta())
#'
#' Takes a simpr class object with metaparameters (e.g., output of simpr::variables() \%>\% simpr::meta()) and generates simulated data to be fit with a statistical model (e.g., lm().
#'
#' @param x simpr class object with metaparamters (e.g., output of simpr::variables() \%>\% simpr::meta())
#' @param reps number of replications to run (integer)
#'
#' @examples
#' gen(simpr, reps = 1000)
#'
#' @return Simulated data
#'
#' @export
gen = function(x, reps) {
  # Create data frame representing all possible values of meta parameter indices
  specs = expand.grid(c(list(rep = 1:reps), x$meta$indices))


  if(!is.null(x$meta$lookup)) {
    ## If there are list elements, join cells representing those list-columns
    ## into specs
    specs = purrr::reduce2(x$meta$lookup,
                           inner_join,
                           .init = specs,
                           .y = names(x$meta$lookup)) # the "by" argument to the join
  }

  ## Generate all replications
  sim_results = specs %>%
    dplyr::group_by_at(.vars = c("rep", names(x$meta$indices))) %>%
    dplyr::do(sim_cell = purrr::pmap(., function(...) {
      eval_environment = rlang::as_environment(list(...), parent = parent.frame())

      df = purrr::map_dfc(x$variables, function(y) {

        eval_fun = purrr::as_mapper(y)
        environment(eval_fun) <- eval_environment

        gen = eval_fun() %>%
          unlist

        varnames = attr(y, "varnames")

        if(is.null(ncol(gen))) {
          gen_df = tibble::as_tibble(gen, .name_repair = "minimal")
          names(gen_df) = varnames
          assign(varnames, gen, envir = eval_environment)

        } else if(length(dim(gen)) > 3) {
          stop("More than 2 dimensional output in variables() not supported")
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
                                    attr(x$variables, "sep"),
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
  attr(sim_results, "meta") = names(x$meta$indices)
  attr(sim_results, "variables") = purrr::map(x$variables, ~ attr(., "varnames")) %>% unlist

  ## Add "simpr_gen" class
  class(sim_results) = c("simpr_gen", class(sim_results))

  sim_results
}


