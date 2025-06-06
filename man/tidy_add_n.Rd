% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_add_n.R
\name{tidy_add_n}
\alias{tidy_add_n}
\title{Add the (weighted) number of observations}
\usage{
tidy_add_n(x, model = tidy_get_model(x))
}
\arguments{
\item{x}{(\code{data.frame})\cr
A tidy tibble as produced by \verb{tidy_*()} functions.}

\item{model}{(a model object, e.g. \code{glm})\cr
The corresponding model, if not attached to \code{x}.}
}
\description{
Add the number of observations in a new column \code{n_obs}, taking into account any
weights if they have been defined.
}
\details{
For continuous variables, it corresponds to all valid observations
contributing to the model.

For categorical variables coded with treatment or sum contrasts,
each model term could be associated to only one level of the original
categorical variable. Therefore, \code{n_obs} will correspond to the number of
observations associated with that level. \code{n_obs} will also be computed for
reference rows. For polynomial contrasts (defined with \code{\link[stats:contrast]{stats::contr.poly()}}),
all levels will contribute to the computation of each model term. Therefore,
\code{n_obs} will be equal to the total number of observations. For Helmert and custom
contrasts, only rows contributing positively (i.e. with a positive contrast)
to the computation of a term will be considered for estimating \code{n_obs}. The
result could therefore be difficult to interpret. For a better understanding
of which observations are taken into account to compute \code{n_obs} values, you
could look at \code{\link[=model_compute_terms_contributions]{model_compute_terms_contributions()}}.

For interaction terms, only rows contributing to all the terms of the
interaction will be considered to compute \code{n_obs}.

For binomial logistic models, \code{tidy_add_n()} will also return the
corresponding number of events (\code{n_event}) for each term, taking into account
any defined weights. Observed proportions could be obtained as \code{n_obs / n_event}.

Similarly, a number of events will be computed for multinomial logistic
models (\code{nnet::multinom()}) for each level of the outcome (\code{y.level}),
corresponding to the number of observations equal to that outcome level.

For Poisson models, \code{n_event} will be equal to the number of counts per term.
In addition, a third column \code{exposure} will be computed. If no offset is
defined, exposure is assumed to be equal to 1 (eventually multiplied by
weights) per observation. If an offset is defined, \code{exposure} will be equal
to the (weighted) sum of the exponential of the offset (as a reminder, to
model the effect of \code{x} on the ratio \code{y / z}, a Poisson model will be defined
as \code{glm(y ~ x + offset(log(z)), family = poisson)}). Observed rates could be
obtained with \code{n_event / exposure}.

For Cox models (\code{\link[survival:coxph]{survival::coxph()}}), an individual could be coded
with several observations (several rows). \code{n_obs} will correspond to the
weighted number of observations which could be different from the number of
individuals \code{n_ind}. \code{tidy_add_n()} will also compute a (weighted) number of
events (\code{n_event}) according to the definition of the \code{\link[survival:Surv]{survival::Surv()}}
object.
Exposure time is also returned in \code{exposure} column. It is equal to the
(weighted) sum of the time variable if only one variable time is passed to
\code{\link[survival:Surv]{survival::Surv()}}, and to the (weighted) sum of \code{time2 - time} if two time
variables are defined in \code{\link[survival:Surv]{survival::Surv()}}.

For competing risk regression models (\code{\link[tidycmprsk:crr]{tidycmprsk::crr()}}), \code{n_event} takes
into account only the event of interest defined by \code{failcode.}

The (weighted) total number of observations (\code{N_obs}), of individuals
(\code{N_ind}), of events (\code{N_event}) and of exposure time (\code{Exposure}) are
stored as attributes of the returned tibble.
}
\examples{
\donttest{
lm(Petal.Length ~ ., data = iris) |>
  tidy_and_attach() |>
  tidy_add_n()

lm(Petal.Length ~ ., data = iris, contrasts = list(Species = contr.sum)) |>
  tidy_and_attach() |>
  tidy_add_n()

lm(Petal.Length ~ ., data = iris, contrasts = list(Species = contr.poly)) |>
  tidy_and_attach() |>
  tidy_add_n()

lm(Petal.Length ~ poly(Sepal.Length, 2), data = iris) |>
  tidy_and_attach() |>
  tidy_add_n()

df <- Titanic |>
  dplyr::as_tibble() |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))

glm(
  Survived ~ Class + Age + Sex,
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
) |>
  tidy_and_attach() |>
  tidy_add_n()

glm(
  Survived ~ Class * (Age:Sex),
  data = df, weights = df$n, family = binomial,
  contrasts = list(Age = contr.sum, Class = "contr.helmert")
) |>
  tidy_and_attach() |>
  tidy_add_n()

glm(response ~ age + grade * trt, gtsummary::trial, family = poisson) |>
  tidy_and_attach() |>
  tidy_add_n()

glm(
  response ~ trt * grade + offset(log(ttdeath)),
  gtsummary::trial,
  family = poisson
) |>
  tidy_and_attach() |>
  tidy_add_n()
}
}
\seealso{
Other tidy_helpers: 
\code{\link{tidy_add_coefficients_type}()},
\code{\link{tidy_add_contrasts}()},
\code{\link{tidy_add_estimate_to_reference_rows}()},
\code{\link{tidy_add_header_rows}()},
\code{\link{tidy_add_pairwise_contrasts}()},
\code{\link{tidy_add_reference_rows}()},
\code{\link{tidy_add_term_labels}()},
\code{\link{tidy_add_variable_labels}()},
\code{\link{tidy_attach_model}()},
\code{\link{tidy_disambiguate_terms}()},
\code{\link{tidy_group_by}()},
\code{\link{tidy_identify_variables}()},
\code{\link{tidy_plus_plus}()},
\code{\link{tidy_remove_intercept}()},
\code{\link{tidy_select_variables}()}
}
\concept{tidy_helpers}
