% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/glance_fits.R
\name{glance_fits}
\alias{glance_fits}
\title{Create tibble of model "glances" (summaries)}
\usage{
glance_fits(obj, ..., .progress = FALSE, .options = furrr_options())
}
\arguments{
\item{obj}{tibble with repetition number,
metaparameters, simulated data, and fitted
models, from
\code{\link[=fit.simpr_tibble]{fit}}}

\item{\dots}{Additional arguments to
\code{broom::\link[generics]{glance}}.}

\item{.progress}{A logical, for whether or not
to print a progress bar for multiprocess,
multisession, and multicore plans .}

\item{.options}{The \code{future} specific
options to use with the workers when using
futures. This must be the result from a call
to
\code{\link[furrr:furrr_options]{furrr_options()}}.}
}
\value{
a tibble with the output of the
  \code{broom::\link[generics]{glance}}
  method for the given object.
}
\description{
Turn fitted models of simulated data (from
\code{\link[=fit.simpr_tibble]{fit}}) into a
tidy tibble of model summaries, each with one line (via
\code{broom::\link[generics]{glance}}).
}
\details{
This the fifth step of the
simulation process: after fitting the model
with \code{\link[=fit.simpr_tibble]{fit}}, now
tidy the model output for further analysis such
as evaluating power.  All model objects should
be supported by
\code{broom::\link[generics]{glance}}.

The output of this function is quite useful comparing
 overall model fits; see
\emph{Examples}. For looking at specific
features of the model such as tests for
individual parameter estimates, use
\code{\link{tidy_fits}}.
}
\examples{
simple_linear_data = specify(a = ~ 2 + rnorm(n),
          b = ~ 5 + 3 * x1 + rnorm(n, 0, sd = 0.5)) \%>\%
  define(n = 100:101) \%>\%
  generate(2)

## Can show tidy output for multiple competing models,
compare_degree = simple_linear_data \%>\%
  fit(linear = ~lm(a ~ b, data = .),
      quadratic = ~lm(a ~ b + I(b^2), data = .)) \%>\%
  glance_fits

compare_degree

## Models can be anything supported by broom::tidy.
cor_vs_lm = simple_linear_data \%>\%
  fit(linear = ~lm(a ~ b, data = .),
      cor = ~ cor.test(.$a, .$b)) \%>\%
  glance_fits

cor_vs_lm # has NA for non-matching terms
}
\seealso{
\code{\link{tidy_fits}} to view model
  components (e.g. parameter estimates),
  \code{\link{apply_fits}} to apply an
  arbitrary function to the fits
}
