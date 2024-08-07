% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/custom_tidiers.R
\name{tidy_multgee}
\alias{tidy_multgee}
\title{Tidy a \code{multgee} model}
\usage{
tidy_multgee(x, conf.int = TRUE, conf.level = 0.95, ...)
}
\arguments{
\item{x}{a \code{multgee::nomLORgee()} or a \code{multgee::ordLORgee()} model}

\item{conf.int}{logical indicating whether or not to include a confidence
interval in the tidied output}

\item{conf.level}{the confidence level to use for the confidence interval}

\item{...}{additional parameters passed to \code{parameters::model_parameters()}}
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
A tidier for models generated with \code{multgee::nomLORgee()} or \code{multgee::ordLORgee()}.
Term names will be updated to be consistent with generic models. The original
term names are preserved in an \code{"original_term"} column.
}
\details{
To be noted, for \code{multgee::nomLORgee()}, the baseline \code{y} category is the
latest modality of \code{y}.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
if (.assert_package("multgee", boolean = TRUE)) {
  library(multgee)

  h <- housing
  h$status <- factor(
    h$y,
    labels = c("street", "community", "independant")
  )

  mod <- multgee::nomLORgee(
    status ~ factor(time) * sec,
    data = h,
    id = id,
    repeated = time,
  )
  mod |> tidy_multgee()

  mod2 <- ordLORgee(
    formula = y ~ factor(time) + factor(trt) + factor(baseline),
    data = multgee::arthritis,
    id = id,
    repeated = time,
    LORstr = "uniform"
  )
  mod2 |> tidy_multgee()
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other custom_tieders: 
\code{\link{tidy_broom}()},
\code{\link{tidy_parameters}()},
\code{\link{tidy_with_broom_or_parameters}()},
\code{\link{tidy_zeroinfl}()}
}
\concept{custom_tieders}
