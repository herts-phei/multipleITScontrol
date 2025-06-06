% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fit_its_model.R
\name{fit_its_model}
\alias{fit_its_model}
\title{fit_its_model}
\usage{
fit_its_model(
  transformed_data,
  impact_model,
  num_interventions,
  method = "REML",
  p = NULL,
  q = NULL,
  ...
)
}
\arguments{
\item{impact_model}{The hypothesized impact model from interventions. Available options are 'level', 'slope', or 'levelscope'.}

\item{num_interventions}{The number of interventions in your transformed data. Should be the vector length of `intervention_dates` passed in `transform_data()`.}

\item{method}{The estimation method for `gls()`, either "REML" (default) or "ML".}

\item{p}{The order of the autoregressive component. Defaults to `NULL`.}

\item{q}{The order of the moving average component. Defaults to `NULL`.}

\item{...}{Additional arguments passed to `nlme::gls()`.}

\item{transformed_df}{Am unmodified data frame created from `transform_data()`.}
}
\value{
A `gls` object of the fitted model.
}
\description{
Fits an interrupted time series model using the \link{nlme} package, defaulting to an autocorrelation-moving average correlation structure of order (p, q)
}
\examples{
fit_its_model(transformed_data = df, impact_model)
}
