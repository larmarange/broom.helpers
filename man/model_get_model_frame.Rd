% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_get_model_frame.R
\name{model_get_model_frame}
\alias{model_get_model_frame}
\alias{model_get_model_frame.default}
\alias{model_get_model_frame.coxph}
\alias{model_get_model_frame.svycoxph}
\alias{model_get_model_frame.survreg}
\alias{model_get_model_frame.biglm}
\alias{model_get_model_frame.model_fit}
\alias{model_get_model_frame.fixest}
\alias{model_get_model_frame.svy_vglm}
\title{Get the model frame of a model}
\usage{
model_get_model_frame(model)

\method{model_get_model_frame}{default}(model)

\method{model_get_model_frame}{coxph}(model)

\method{model_get_model_frame}{svycoxph}(model)

\method{model_get_model_frame}{survreg}(model)

\method{model_get_model_frame}{biglm}(model)

\method{model_get_model_frame}{model_fit}(model)

\method{model_get_model_frame}{fixest}(model)

\method{model_get_model_frame}{svy_vglm}(model)
}
\arguments{
\item{model}{(a model object, e.g. \code{glm})\cr
A model object.}
}
\description{
The structure of the object returned by \code{\link[stats:model.frame]{stats::model.frame()}}
could slightly differ for certain types of models.
\code{model_get_model_frame()} will always return an object
with the same data structure or \code{NULL} if it is not possible
to compute model frame from \code{model}.
}
\examples{
lm(hp ~ mpg + factor(cyl), mtcars) |>
  model_get_model_frame() |>
  head()
}
\seealso{
\code{\link[stats:model.frame]{stats::model.frame()}}

Other model_helpers: 
\code{\link{model_compute_terms_contributions}()},
\code{\link{model_get_assign}()},
\code{\link{model_get_coefficients_type}()},
\code{\link{model_get_contrasts}()},
\code{\link{model_get_model}()},
\code{\link{model_get_model_matrix}()},
\code{\link{model_get_n}()},
\code{\link{model_get_nlevels}()},
\code{\link{model_get_offset}()},
\code{\link{model_get_pairwise_contrasts}()},
\code{\link{model_get_response}()},
\code{\link{model_get_response_variable}()},
\code{\link{model_get_terms}()},
\code{\link{model_get_weights}()},
\code{\link{model_get_xlevels}()},
\code{\link{model_identify_variables}()},
\code{\link{model_list_contrasts}()},
\code{\link{model_list_higher_order_variables}()},
\code{\link{model_list_terms_levels}()},
\code{\link{model_list_variables}()}
}
\concept{model_helpers}
