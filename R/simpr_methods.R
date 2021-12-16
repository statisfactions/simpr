#' @export
print.simpr_tibble = function(x, ...) {
  sim_name = get_sim_name(x)

  sim_1 = x[[sim_name]][[1]]
  fit_names = attr(x, "fits")

  if(is.null(sim_1)) {
    y = list(`tibble` = x)
  } else {
    y = list(x, sim_1)
    names(y) = c("full tibble", paste0(sim_name, "[[1]]"))
  }

  if(!is.null(fit_names) && all(fit_names %in% names(x))) {
    fit_cols = x %>% as.data.frame %>% tibble::as_tibble() %>%
    dplyr::select(tidyselect::one_of(c(attr(x, "fits")))) %>%
      purrr::map(1)
    names(fit_cols) = paste0(names(fit_cols), "[[1]]")
    y = c(y, fit_cols)
  }
  purrr::imap(y, function(z, znm) {
    cat(znm, "\n--------------------------\n", sep = "")
    if(is.data.frame(z))
      print(tibble::as_tibble(as.data.frame(z), ..., bare_tibble = TRUE))
    else
      print(z)
    cat("\n", sep = "")
  })
}
