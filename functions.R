
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
  specs = expand.grid(c(simpr$meta, list(rep = 1:reps))) 
  
  ## Generate all replications
  sim_results = specs %>% 
    group_by_all() %>%
    do(sim_cell = pmap(., function(...) {
      meta_cell = list(...)
      attach(meta_cell)

      df = imap_dfc(simpr$variables, function(x, y) {
          
          eval_fn = as_mapper(x)
          
          gen = eval_fn() %>% 
            unlist
          
          assign(y, gen, envir = .GlobalEnv)
          gen
        })
      
      detach(meta_cell)
      df
      
    })) %>% unnest
  attr(sim_results, "meta") = names(simpr$meta)
  attr(sim_results, "variables") = names(simpr$variables)
  
  sim_results
}

fit = function(simpr_gen, ...) {
  
  fit_functions = list(...)
  
  simpr_mod = simpr_gen
  
  for(i in names(fit_functions))
    simpr_mod[[i]] = map(simpr_mod$sim_cell, fit_functions[[1]])
  
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

calc_tidy = function(simpr_mod) {
  simpr_meta = simpr_mod %>% select(one_of(c(attr(simpr_mod, "meta"), "rep")))
  simpr_mods = simpr_mod %>% 
    select(one_of(c(attr(simpr_mod, "fits"))))
  
  
  
  simpr_tidy = map_dfr(simpr_mods, ~ map_dfr(., tidy), .id = "Source")
  
  # THIS IS A HACK
  rep_factor = nrow(simpr_tidy)/nrow(simpr_meta)
  meta_rep = simpr_meta[rep(1:nrow(simpr_meta), rep_factor),]
  
    bind_cols(meta_rep, simpr_tidy)
  
  
}

calc_glance = function(simpr_mod) {
  simpr_meta = simpr_mod %>% select(-mod)
  
  simpr_glance = simpr_mod %>% 
    do(glance = glance(.$mod)) %>% 
    bind_cols(simpr_meta, .) %>% 
    unnest
  
}

