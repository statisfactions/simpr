
# setup -------------------------------------------------------------------

library(tidyverse)
library(broom)
# truth -------------------------------------------------------------------
## List that sets the names of the input objects

variables = function(...) {
  # Specify functions to generate variables
  out = list(variables = list(...))
  class(out) = "simpr"
  out
}

meta = function(x, ...) {
 # Specify metaparameters to vary
  x$meta = list(...)
  x
}

gen = function(simpr, reps) {
  ## Create labeled list representing all possible values of meta parameters
  specs = expand.grid(simpr$meta) 
  
  ## Generate all replications
  sim_results = specs %>% 
    group_by_all() %>%
    do(sim_cell = pmap(., function(...) {
      meta_cell = list(...)
      attach(meta_cell)
      
      spc_mat = replicate(reps, {
        df = imap_dfc(simpr$variables, function(x, y) {
          
          eval_fn = as_mapper(x)
          
          gen = eval_fn() %>% 
            unlist
          
          assign(y, gen, envir = .GlobalEnv)
          gen
        })
        rm(envir = .GlobalEnv, list = names(simpr$variables))
        df
      }, simplify = FALSE)
      detach(meta_cell)
      
      bind_rows(spc_mat, .id = "rep") %>% 
        mutate(rep = as.integer(rep))
      
    })) %>% 
    unnest  %>% 
    unnest %>% 
    arrange_at(c(names(simpr$meta), "rep"))
  attr(sim_results, "meta") = names(simpr$meta)
  attr(sim_results, "variables") = names(simpr$variables)
  
  sim_results
}

fit = function(simpr_gen, formula) {
  eval_fun = as_mapper(formula)
  simpr_mod = simpr_gen %>% 
    group_by_at(c(attr(simpr_gen, "meta"), "rep")) %>% 
    do(mod = eval_fun(.))
  
  attr(simpr_mod, "meta") = attr(simpr_gen, "meta")
  attr(simpr_mod, "variables") = attr(simpr_gen, "variables")
  
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

calc_tidy = function(simpr_mod) {
  simpr_meta = simpr_mod %>% select(-mod)
  
  simpr_tidy = simpr_mod %>% 
    do(tidy = tidy(.$mod)) %>% 
    bind_cols(simpr_meta, .) %>% 
    unnest
  
}

calc_glance = function(simpr_mod) {
  simpr_meta = simpr_mod %>% select(-mod)
  
  simpr_glance = simpr_mod %>% 
    do(glance = glance(.$mod)) %>% 
    bind_cols(simpr_meta, .) %>% 
    unnest
  
}

