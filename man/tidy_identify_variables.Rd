% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_identify_variables.R
\name{tidy_identify_variables}
\alias{tidy_identify_variables}
\title{Identify the variable corresponding to each model coefficient}
\usage{
tidy_identify_variables(x, model = tidy_get_model(x))
}
\arguments{
\item{x}{a tidy tibble}

\item{model}{the corresponding model, if not attached to \code{x}}
}
\description{
\code{tidy_identify_variables()} will add to the tidy tibble
three additional columns: \code{variable}, \code{var_class} and \code{var_type}.
}
\details{
It will also identify interaction terms and intercept(s).
\code{var_type} could be \code{"continuous"}, \code{"categorical"}, \code{"intercept"}
or \code{"interaction"}.
}
\examples{
Titanic \%>\%
  dplyr::as_tibble() \%>\%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) \%>\%
  glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) \%>\%
  tidy_and_attach() \%>\%
  tidy_identify_variables()

lm(
  Sepal.Length ~ poly(Sepal.Width, 2) + Species,
  data = iris,
  contrasts = list(Species = contr.sum)
) \%>\%
tidy_and_attach(conf.int = TRUE) \%>\%
tidy_identify_variables()
}
\seealso{
\code{\link[=model_identify_variables]{model_identify_variables()}}

Other tidy_helpers: 
\code{\link{tidy_add_contrasts}()},
\code{\link{tidy_add_estimate_to_reference_rows}()},
\code{\link{tidy_add_header_rows}()},
\code{\link{tidy_add_reference_rows}()},
\code{\link{tidy_add_term_labels}()},
\code{\link{tidy_add_variable_labels}()},
\code{\link{tidy_attach_model}()},
\code{\link{tidy_plus_plus}()},
\code{\link{tidy_remove_intercept}()}
}
\concept{tidy_helpers}