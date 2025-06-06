% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_tidiers.R
\name{tidy_zeroinfl}
\alias{tidy_zeroinfl}
\title{Tidy a \code{zeroinfl} or a \code{hurdle} model}
\usage{
tidy_zeroinfl(x, conf.int = TRUE, conf.level = 0.95, component = NULL, ...)
}
\arguments{
\item{x}{(\code{zeroinfl} or \code{hurdle})\cr
A \code{pscl::zeroinfl()} or a \code{pscl::hurdle()} model.}

\item{conf.int}{(\code{logical})\cr
Whether or not to include a confidence interval in the tidied output.}

\item{conf.level}{(\code{numeric})\cr
The confidence level to use for the confidence interval (between \code{0} ans \code{1}).}

\item{component}{(\code{string})\cr
\code{NULL} or one of \code{"all"}, \code{"conditional"}, \code{"zi"}, or \code{"zero_inflated"}.}

\item{...}{Additional parameters passed to \code{parameters::model_parameters()}.}
}
\description{
A tidier for models generated with \code{pscl::zeroinfl()} or \code{pscl::hurdle()}.
Term names will be updated to be consistent with generic models. The original
term names are preserved in an \code{"original_term"} column.
}
\examples{
\dontshow{if (.assert_package("pscl", boolean = TRUE)) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
  library(pscl)
  mod <- zeroinfl(
    art ~ fem + mar + phd,
    data = pscl::bioChemists
  )

  mod |> tidy_zeroinfl(exponentiate = TRUE)
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other custom_tieders: 
\code{\link{tidy_broom}()},
\code{\link{tidy_multgee}()},
\code{\link{tidy_parameters}()},
\code{\link{tidy_svy_vglm}()},
\code{\link{tidy_vgam}()},
\code{\link{tidy_with_broom_or_parameters}()}
}
\concept{custom_tieders}
