% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_add_variable_labels.R
\name{tidy_add_variable_labels}
\alias{tidy_add_variable_labels}
\title{Add variable labels}
\usage{
tidy_add_variable_labels(
  x,
  labels = NULL,
  interaction_sep = " * ",
  instrumental_suffix = " (instrumental)",
  model = tidy_get_model(x)
)
}
\arguments{
\item{x}{(\code{data.frame})\cr
A tidy tibble as produced by \verb{tidy_*()} functions.}

\item{labels}{(\code{\link[gtsummary:syntax]{formula-list-selector}})\cr
An optional named list or a named vector of custom variable labels.}

\item{interaction_sep}{(\code{string})\cr
Separator for interaction terms.}

\item{instrumental_suffix}{(\code{string})\cr
Suffix added to variable labels for instrumental variables (\code{fixest} models).
\code{NULL} to add nothing.}

\item{model}{(a model object, e.g. \code{glm})\cr
The corresponding model, if not attached to \code{x}.}
}
\description{
Will add variable labels in a \code{var_label} column, based on:
\enumerate{
\item labels provided in \code{labels} argument if provided;
\item variable labels defined in the original data frame with
the \code{label} attribute (cf. \code{\link[labelled:var_label]{labelled::var_label()}});
\item variable name otherwise.
}
}
\details{
If the \code{variable} column is not yet available in \code{x},
\code{\link[=tidy_identify_variables]{tidy_identify_variables()}} will be automatically applied.

It is possible to pass a custom label for an interaction
term in \code{labels} (see examples).
}
\examples{
df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) |>
  labelled::set_variable_labels(
    Class = "Passenger's class",
    Sex = "Sex"
  )

glm(Survived ~ Class * Age * Sex, data = df, weights = df$n, family = binomial) |>
  tidy_and_attach() |>
  tidy_add_variable_labels(
    labels = list(
      "(Intercept)" ~ "Custom intercept",
      Sex ~ "Gender",
      "Class:Age" ~ "Custom label"
    )
  )
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
\code{\link{tidy_attach_model}()},
\code{\link{tidy_disambiguate_terms}()},
\code{\link{tidy_group_by}()},
\code{\link{tidy_identify_variables}()},
\code{\link{tidy_plus_plus}()},
\code{\link{tidy_remove_intercept}()},
\code{\link{tidy_select_variables}()}
}
\concept{tidy_helpers}
