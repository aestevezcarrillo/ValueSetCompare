% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empirical_comparison.R
\name{.extract_columns}
\alias{.extract_columns}
\title{.extract_columns}
\usage{
.extract_columns(
  df,
  column_names = c("VAS", "utility_3L", "utility_5L", "utility_xw"),
  sample_indices
)
}
\arguments{
\item{df}{A data frame from which to extract columns.}

\item{column_names}{A character vector of the names of the columns to be extracted from df.}

\item{sample_indices}{An array of sampled indices, each element of the array represents the index of a row in df.}
}
\value{
A list of arrays, with each array  containing the values of the respective column for the sampled indices. 
  Each array has the same dimensions as sampledIndices. The names of the list elements are the names of column_names.
}
\description{
This function extracts the specified columns from a data frame for the given samples.
}
\keyword{internal}
