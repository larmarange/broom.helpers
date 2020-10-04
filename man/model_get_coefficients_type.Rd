% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_get_coefficients_type.R
\name{model_get_coefficients_type}
\alias{model_get_coefficients_type}
\alias{model_get_coefficients_type.default}
\alias{model_get_coefficients_type.glm}
\alias{model_get_coefficients_type.geeglm}
\alias{model_get_coefficients_type.glmerMod}
\alias{model_get_coefficients_type.clogit}
\alias{model_get_coefficients_type.polr}
\alias{model_get_coefficients_type.multinom}
\alias{model_get_coefficients_type.clm}
\alias{model_get_coefficients_type.clmm}
\alias{model_get_coefficients_type.coxph}
\title{Get coefficient type}
\usage{
model_get_coefficients_type(model)

\method{model_get_coefficients_type}{default}(model)

\method{model_get_coefficients_type}{glm}(model)

\method{model_get_coefficients_type}{geeglm}(model)

\method{model_get_coefficients_type}{glmerMod}(model)

\method{model_get_coefficients_type}{clogit}(model)

\method{model_get_coefficients_type}{polr}(model)

\method{model_get_coefficients_type}{multinom}(model)

\method{model_get_coefficients_type}{clm}(model)

\method{model_get_coefficients_type}{clmm}(model)

\method{model_get_coefficients_type}{coxph}(model)
}
\arguments{
\item{model}{a model object}
}
\description{
Indicate the type of coefficient among "generic", "logistic",
"poisson" and "prop_hazard".
}
\examples{
lm(hp ~ mpg + factor(cyl), mtcars) \%>\%
  model_get_coefficients_type()

Titanic \%>\%
  dplyr::as_tibble() \%>\%
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes"))) \%>\%
  glm(Survived ~ Class + Age * Sex, data = ., weights = .$n, family = binomial) \%>\%
  model_get_coefficients_type()
}
\seealso{
Other model_helpers: 
\code{\link{model_get_contrasts}()},
\code{\link{model_get_model_frame}()},
\code{\link{model_get_model_matrix}()},
\code{\link{model_get_xlevels}()},
\code{\link{model_identify_variables}()},
\code{\link{model_list_contrasts}()},
\code{\link{model_list_terms_levels}()},
\code{\link{model_list_variables}()}
}
\concept{model_helpers}