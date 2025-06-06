% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/marginal_tidiers.R
\name{tidy_marginal_predictions}
\alias{tidy_marginal_predictions}
\alias{variables_to_predict}
\alias{plot_marginal_predictions}
\title{Marginal Predictions with \code{marginaleffects::avg_predictions()}}
\usage{
tidy_marginal_predictions(
  x,
  variables_list = "auto",
  conf.int = TRUE,
  conf.level = 0.95,
  ...
)

variables_to_predict(
  model,
  interactions = TRUE,
  categorical = unique,
  continuous = stats::fivenum
)

plot_marginal_predictions(x, variables_list = "auto", conf.level = 0.95, ...)
}
\arguments{
\item{x}{(a model object, e.g. \code{glm})\cr
A model to be tidied.}

\item{variables_list}{(\code{list} or \code{string})\cr
A list whose elements will be sequentially passed to
\code{variables} in \code{marginaleffects::avg_predictions()} (see details below);
alternatively, it could also be the string \code{"auto"} (default) or
\code{"no_interaction"}.}

\item{conf.int}{(\code{logical})\cr
Whether or not to include a confidence interval in the tidied output.}

\item{conf.level}{(\code{numeric})\cr
The confidence level to use for the confidence interval (between \code{0} ans \code{1}).}

\item{...}{Additional parameters passed to
\code{marginaleffects::avg_predictions()}.}

\item{model}{(a model object, e.g. \code{glm})\cr
A model.}

\item{interactions}{(\code{logical})\cr
Should combinations of variables corresponding to
interactions be returned?}

\item{categorical}{(\code{\link[marginaleffects:predictions]{predictor values}})\cr
Default values for categorical variables.}

\item{continuous}{(\code{\link[marginaleffects:predictions]{predictor values}})\cr
Default values for continuous variables.}
}
\description{
Use \code{marginaleffects::avg_predictions()} to estimate marginal predictions for
each variable of a model and return a tibble tidied in a way that it could
be used by \code{broom.helpers} functions.
See \code{marginaleffects::avg_predictions()} for a list of supported models.
}
\details{
Marginal predictions are obtained by calling, for each variable,
\code{marginaleffects::avg_predictions()} with the same variable being used for
the \code{variables} and the \code{by} argument.

Considering a categorical variable named \code{cat}, \code{tidy_marginal_predictions()}
will call \code{avg_predictions(model, variables = list(cat = unique), by = "cat")}
to obtain average marginal predictions for this variable.

Considering a continuous variable named \code{cont}, \code{tidy_marginal_predictions()}
will call \code{avg_predictions(model, variables = list(cont = "fivenum"), by = "cont")}
to obtain average marginal predictions for this variable at the minimum, the
first quartile, the median, the third quartile and the maximum of the observed
values of \code{cont}.

By default, \emph{average marginal predictions} are computed: predictions are made
using a counterfactual grid for each value of the variable of interest,
before averaging the results. \emph{Marginal predictions at the mean} could be
obtained by indicating \code{newdata = "mean"}. Other assumptions are possible,
see the help file of \code{marginaleffects::avg_predictions()}.

\code{tidy_marginal_predictions()} will compute marginal predictions for each
variable or combination of variables, before stacking the results in a unique
tibble. This is why \code{tidy_marginal_predictions()} has a \code{variables_list}
argument consisting of a list of specifications that will be passed
sequentially to the \code{variables} argument of \code{marginaleffects::avg_predictions()}.

The helper function \code{variables_to_predict()} could be used to automatically
generate a suitable list to be used with \code{variables_list}. By default, all
unique values are retained for categorical variables and \code{fivenum} (i.e.
Tukey's five numbers, minimum, quartiles and maximum) for continuous variables.
When \code{interactions = FALSE}, \code{variables_to_predict()} will return a list of
all individual variables used in the model. If \code{interactions = FALSE}, it
will search for higher order combinations of variables (see
\code{model_list_higher_order_variables()}).

\code{variables_list}'s default value, \code{"auto"}, calls
\code{variables_to_predict(interactions = TRUE)} while \code{"no_interaction"} is a
shortcut for \code{variables_to_predict(interactions = FALSE)}.

You can also provide custom specifications (see examples).

\code{plot_marginal_predictions()} works in a similar way and returns a list of
plots that could be combined with \code{patchwork::wrap_plots()} (see examples).

For more information, see \code{vignette("marginal_tidiers", "broom.helpers")}.
}
\examples{
\dontshow{if (.assert_package("marginaleffects", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# example code

\donttest{
# Average Marginal Predictions
df <- Titanic |>
  dplyr::as_tibble() |>
  tidyr::uncount(n) |>
  dplyr::mutate(Survived = factor(Survived, c("No", "Yes")))
mod <- glm(
  Survived ~ Class + Age + Sex,
  data = df, family = binomial
)
tidy_marginal_predictions(mod)
tidy_plus_plus(mod, tidy_fun = tidy_marginal_predictions)
if (require("patchwork")) {
  plot_marginal_predictions(mod) |> patchwork::wrap_plots()
  plot_marginal_predictions(mod) |>
    patchwork::wrap_plots() &
    ggplot2::scale_y_continuous(limits = c(0, 1), label = scales::percent)
}

mod2 <- lm(Petal.Length ~ poly(Petal.Width, 2) + Species, data = iris)
tidy_marginal_predictions(mod2)
if (require("patchwork")) {
  plot_marginal_predictions(mod2) |> patchwork::wrap_plots()
}
tidy_marginal_predictions(
  mod2,
  variables_list = variables_to_predict(mod2, continuous = "threenum")
)
tidy_marginal_predictions(
  mod2,
  variables_list = list(
    list(Petal.Width = c(0, 1, 2, 3)),
    list(Species = unique)
  )
)
tidy_marginal_predictions(
  mod2,
  variables_list = list(list(Species = unique, Petal.Width = 1:3))
)

# Model with interactions
mod3 <- glm(
  Survived ~ Sex * Age + Class,
  data = df, family = binomial
)
tidy_marginal_predictions(mod3)
tidy_marginal_predictions(mod3, "no_interaction")
if (require("patchwork")) {
  plot_marginal_predictions(mod3) |>
    patchwork::wrap_plots()
  plot_marginal_predictions(mod3, "no_interaction") |>
    patchwork::wrap_plots()
}
tidy_marginal_predictions(
  mod3,
  variables_list = list(
    list(Class = unique, Sex = "Female"),
    list(Age = unique)
  )
)

# Marginal Predictions at the Mean
tidy_marginal_predictions(mod, newdata = "mean")
if (require("patchwork")) {
  plot_marginal_predictions(mod, newdata = "mean") |>
    patchwork::wrap_plots()
}
}
\dontshow{\}) # examplesIf}
}
\seealso{
\code{marginaleffects::avg_predictions()}

Other marginal_tieders: 
\code{\link{tidy_all_effects}()},
\code{\link{tidy_avg_comparisons}()},
\code{\link{tidy_avg_slopes}()},
\code{\link{tidy_ggpredict}()},
\code{\link{tidy_marginal_contrasts}()},
\code{\link{tidy_margins}()}
}
\concept{marginal_tieders}
