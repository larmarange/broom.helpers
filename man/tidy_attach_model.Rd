% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_and_attach.R
\name{tidy_attach_model}
\alias{tidy_attach_model}
\alias{tidy_and_attach}
\alias{tidy_get_model}
\alias{tidy_detach_model}
\title{Attach a full model to the tibble of model terms}
\usage{
tidy_attach_model(x, model, .attributes = NULL)

tidy_and_attach(
  model,
  tidy_fun = tidy_with_broom_or_parameters,
  conf.int = TRUE,
  conf.level = 0.95,
  exponentiate = FALSE,
  model_matrix_attr = TRUE,
  ...
)

tidy_get_model(x)

tidy_detach_model(x)
}
\arguments{
\item{x}{(\code{data.frame})\cr
A tidy tibble as produced by \verb{tidy_*()} functions.}

\item{model}{(a model object, e.g. \code{glm})\cr
A model to be attached/tidied.}

\item{.attributes}{(\code{list})\cr
Named list of additional attributes to be attached to \code{x}.}

\item{tidy_fun}{(\code{function})\cr
Option to specify a custom tidier function.}

\item{conf.int}{(\code{logical})\cr
Should confidence intervals be computed? (see \code{\link[broom:reexports]{broom::tidy()}})}

\item{conf.level}{(\code{numeric})\cr
Level of confidence for confidence intervals (default: 95\%).}

\item{exponentiate}{(\code{logical})\cr
Whether or not to exponentiate the coefficient estimates.
This is typical for logistic, Poisson and Cox models,
but a bad idea if there is no log or logit link; defaults to \code{FALSE}.}

\item{model_matrix_attr}{(\code{logical})\cr
Whether model frame and model matrix should be added as attributes of
\code{model} (respectively named \code{"model_frame"} and \code{"model_matrix"}) and
passed through}

\item{...}{Other arguments passed to \code{tidy_fun()}.}
}
\description{
To facilitate the use of broom helpers with pipe, it is recommended to
attach the original model as an attribute to the tibble of model terms
generated by \code{broom::tidy()}.
}
\details{
\code{tidy_attach_model()} attach the model to a tibble already generated while
\code{tidy_and_attach()} will apply \code{broom::tidy()} and attach the model.

Use \code{tidy_get_model()} to get the model attached to the tibble and
\code{tidy_detach_model()} to remove the attribute containing the model.
}
\examples{
mod <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
tt <- mod |>
  tidy_and_attach(conf.int = TRUE)
tt
tidy_get_model(tt)
}
\seealso{
Other tidy_helpers: 
\code{\link{tidy_add_coefficients_type}()},
\code{\link{tidy_add_contrasts}()},
\code{\link{tidy_add_estimate_to_reference_rows}()},
\code{\link{tidy_add_header_rows}()},
\code{\link{tidy_add_n}()},
\code{\link{tidy_add_pairwise_contrasts}()},
\code{\link{tidy_add_reference_rows}()},
\code{\link{tidy_add_term_labels}()},
\code{\link{tidy_add_variable_labels}()},
\code{\link{tidy_disambiguate_terms}()},
\code{\link{tidy_group_by}()},
\code{\link{tidy_identify_variables}()},
\code{\link{tidy_plus_plus}()},
\code{\link{tidy_remove_intercept}()},
\code{\link{tidy_select_variables}()}
}
\concept{tidy_helpers}
