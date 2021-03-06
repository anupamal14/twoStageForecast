% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoTwoStage.R
\name{auto.twoStage}
\alias{auto.twoStage}
\title{Two-stage methods for forecasting time series with mulitple
levels of seasonality}
\usage{
auto.twoStage(
  d,
  n = NULL,
  p = NULL,
  seas_periods = NULL,
  regMat = NULL,
  plotFlag = FALSE
)
}
\arguments{
\item{d}{the actual data.}

\item{n}{Number of samples in the model period.}

\item{p}{Number of samples in the holdout period.}

\item{seas_periods}{periods corresponding to levels of
seasonality in the data}

\item{regMat}{regression matrix with explanatory variables as columns}

\item{plotFlag}{whether plot is required at end of stage 1. Default is FALSE.}
}
\description{
Estimates the seasonality at different levels by
subtracting the mean for additive seasonality
(dividing for multiplicative) and then averaging across
the entire data.
}
\examples{

}
