#' @export
print.simpr_tibble = function(x, ...) {
  sim_name = get_sim_name(x)
  y = list(x, x[[sim_name]][[1]])
  names(y) = c("full tibble", paste0(sim_name, "[[1]]"))

  purrr::imap(y, function(z, znm) {
    cat(znm, "\n--------------------------\n", sep = "")
    print(dplyr::as_tibble(z, ...))
    cat("\n", sep = "")
  })
}

#' @export
print.simpr_sims = function(x, ...) {
  sim_name = get_sim_name(x)
  y = list(x, x[[sim_name]][[1]])
  names(y) = c("full tibble", paste0(sim_name, "[[1]]"))

  purrr::imap(rev(y), function(z, znm) {
    cat(znm, "\n--------------------------\n", sep = "")
    print(dplyr::as_tibble(z, ...))
    cat("\n", sep = "")
  })
}
