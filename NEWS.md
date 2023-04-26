# simpr 0.2.6
- Removed "dontrun" and tests relating to global environment (which ironically were testing whether the package touched the global environment by touching the global environment)

# simpr 0.2.5
- Loosen tests that were giving errors on some linux systems (locale-dependent newline issues)

# simpr 0.2.4
- Added lifecycle dependency after CRAN test failures (checking dependencies in R code ... WARNING '::' or ':::' import not declared from: ‘lifecycle’)
- Fix double-sided formula issue with purrr deprecating calls
- Add support for .list argument to pass arbitrary lists of parameters to define()

# simpr 0.2.3
Fixes test failures in CRAN version simpr 0.2.2 after 2022-12-20 release of dependency purrr 1.0.0, and update to tidyselect 1.2.0.

# simpr 0.2.2
Update `pivot_wider.simpr_sims()` and `pivot_wider.simpr_spec()` to match new arguments in `pivot_wider()` generic in `tidyr` 1.2.0. (Issue #69; PR #68, @DavisVaughan)

# simpr 0.2.1
Added return values to documentation for `apply_fits()`, `simpr_spec()`, and all `dplyr` & `tidyr` methods.

# simpr 0.2.0
Initial CRAN submission.
