% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empirical_comparison.R
\name{.gen_samples}
\alias{.gen_samples}
\title{.gen_samples}
\usage{
.gen_samples(
  df,
  weight_column = "VAS",
  weight_range = c(0:100),
  weight_values = NULL,
  weight_function = .makeWeightsMixed,
  sample_size = 1000,
  number_of_samples = 1000
)
}
\arguments{
\item{df}{A data frame from which the samples are to be generated.}

\item{weight_column}{A string specifying the name of the column to use for weighting.Defaults to "VAS".}

\item{weight_range}{A numeric vector indicating the range of weight_column, default is 0 to 100.}

\item{weight_values}{A numeric vector specifying the weight values of interest. Defaults to c(0, 25, 50, 75, 100).}

\item{weight_function}{A function used to compute weights. Defaults to "makeWeightsTriangular".}

\item{sample_size}{An integer specifying the size of each sample. Defaults to 1000.}

\item{number_of_samples}{An integer specifying the number of samples to generate. Defaults to 1000.}
}
\value{
An array containing the indices of the sampled rows in the original data frame. The dimensions of the array are determined by the number of weight values and the number of samples.
}
\description{
This function generates stratified samples from a specified dataframe.
}
\keyword{internal}
