library(tidyverse)
library(glue)

search_pkgs = c("dplyr", "tidyr")
names(search_pkgs) = search_pkgs
## all tbl_df methods also appear as data.frame methods, so only use those
df_methods = map(search_pkgs, ~ ls(getNamespace(.x), all.names = TRUE) %>%
                   str_subset("\\.data\\.frame$") %>%
                   str_remove("\\.data\\.frame$"))

## Only include methods for generics that are actually exported in these packages
df_methods_package = imap(df_methods, ~ .x[.x %in% ls(glue("package:{.y}"))])

## Get the function signature for a function x, specified as a character string
getsignature = function(x) {
  as.call(c(as.symbol(x), formals(x))) %>%
    deparse %>%
    str_replace_all(" = ,", ",") %>%
    str_replace_all(" = \\)", ")") %>%
    str_replace(glue("^{x}"), "function") %>%
    paste(collapse = "\n")
}

## Print roxygen documentation for params
get_params = function(x) {
  args = names(formals(x)) %>% str_replace("^\\.\\.\\.$", "\\\\dots")
  map_chr(args, ~ glue("#' @param {.x} See original function documentation")) %>%
    paste0(collapse = "\n")
}

## Get the name of the first argument of a function
arg1 = function(x) {
  names(formals(x))[1]
}



build_simpr_methods = function(x, pkg) {
  cat(pkg, x, "\n")
  glue("
  #' @rdname tidyverse_verbs
  {get_params(x)}
  #' @export
  {x}.simpr_sims = {getsignature(x)} {{
  mc = match.call()
  mc[[1]] = quote({x})

  {arg1(x)}[[get_sim_name({arg1(x)})]] =  purrr::map({arg1(x)}[[get_sim_name({arg1(x)})]],
                                             ~ eval(mc))
  {arg1(x)}
  }}

  #' @rdname tidyverse_verbs
  {get_params(x)}
  #' @export
  {x}.simpr_spec = {getsignature(x)} {{
  mc = match.call()

  add_call({arg1(x)}, mc, '{x}', replace_arg = 2)
  }}
  ")[1] # not sure why this is necessary
}

build_simpr_methods("add_count", "dplyr")
built = imap(df_methods_package, ~ map_chr(.x, build_simpr_methods, pkg = .y)) %>%
  unlist %>%
  paste(collapse = "\n")

cat("#' Simpr methods for tidyverse verbs
#'
#' These are simpr-compatible methods for generic
#' \\code{dplyr} and \\code{tidyr} verbs. The
#' user is not expected to call these directly.
#'
#' See original function documentation for details of the functions
#'
#' @name tidyverse_verbs
NULL\n",
    built,
    file = "R/tidyverse_verb_methods.R", sep = "\n")
