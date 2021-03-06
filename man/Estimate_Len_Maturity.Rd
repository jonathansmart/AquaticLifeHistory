% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Maturity_functions.R
\name{Estimate_Len_Maturity}
\alias{Estimate_Len_Maturity}
\title{Estimate length-at-maturity}
\usage{
Estimate_Len_Maturity(
  data,
  error.structure = "binomial",
  n.bootstraps = 1000,
  bin.width = NA,
  display.points = FALSE,
  return = "parameters"
)
}
\arguments{
\item{data}{A dataframe that includes length and a binary maturity status (immature = 0 and mature = 1).
Columns should be named "Length" and "Maturity" but the function is robust enough to accept some reasonable variations to these}

\item{error.structure}{The distribution for the glm used to produce the logistic ogive. Must be either "binomial"
for binary data or "quasibinomial" for binned maturity at length. Proportion mature at each length bin is automatically calculated within the function}

\item{n.bootstraps}{Number of bootstrap iterations required to produce 95\% confidence intervals about the logistic ogive}

\item{bin.width}{The width of the length-class bins used for a quasibinomial logistic model. These should on the same unit as the length data.
The y axis on any plots will automatically scale to the correct unit ("cm" or "mm")}

\item{display.points}{Should the raw data be plotted for the binomial model?}

\item{return}{Either: \describe{
\item{parameters}{The estimated logistic parameters and their standard error (L50 and L95)}
\item{estimates}{The logistic ogive predictions with 95 percent confidence intervals (useful for creating ones own plots)}
\item{plot}{a ggplot object of the logistic ogive.}
}}
}
\value{
Either: \describe{
\item{parameters}{a dataframe of the estimated logistic parameters and their standard error (L50 and L95)}
\item{estimates}{a dataframe of logistic ogive predictions with 95 percent confidence intervals}
\item{plot}{a ggplot object of the logistic ogive. If binned length classes are used, this includes a bar plot of proportional maturity }
}
}
\description{
Length-at-maturity is estimated from binary maturity data using a logistic ogive.
Two options are availale depending on error structure. If binary data are used then a binomial
error stucture is required. If the user wishes to bin the data by length class then a quasibinomial error
structure is needed with the data weighted by the sample size of each bin. This is handled automatically by the function.
}
