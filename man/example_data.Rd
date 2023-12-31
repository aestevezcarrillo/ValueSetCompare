% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{example_data}
\alias{example_data}
\title{example_data}
\format{
A data frame with 3749 rows and 20 variables:
\describe{
  \item{\code{conditiongroups}}{integer An integer variable representing different health conditions. Values range from 1 to 10, where each value corresponds to the following conditions, in order:
    \enumerate{
      \item \code{1}: COPD/Asthma
      \item \code{2}: Diabetes
      \item \code{3}: Liver disease
      \item \code{4}: RA/A
      \item \code{5}: CVD
      \item \code{6}: Stroke
      \item \code{7}: Depression
      \item \code{8}: Personality Disorder
      \item \code{9}: Other
      \item \code{10}: Students
    }}
  \item{\code{studyID}}{integer Identification of the origical study}
  \item{\code{profile3L}}{integer EQ-5D-3L Health state}
  \item{\code{profile5L}}{integer EQ-5D-5L Health state}
  \item{\code{respID}}{integer Participant's ID in the original study}
  \item{\code{condition}}{integer An integer variable representing different health conditions before grouping}
  \item{\code{age}}{integer Participant's age in years}
  \item{\code{gender}}{integer Participant's gender (Female / Male)}
  \item{\code{education}}{integer Participant's educational level}
  \item{\code{mobility}}{integer EQ-5D-3L Mobility dimension}
  \item{\code{selfcare}}{integer EQ-5D-3L Self-Care dimension}
  \item{\code{activity}}{integer EQ-5D-3L Usual activities dimension}
  \item{\code{pain}}{integer EQ-5D-3L Pain/discomfort dimension}
  \item{\code{anxiety}}{integer EQ-5D-3L Anxiety/depression dimension}
  \item{\code{VAS}}{integer Value of the VAS scale measurememnt}
  \item{\code{mobility5L}}{integer EQ-5D-5L Mobility dimension}
  \item{\code{selfcare5L}}{integer EQ-5D-5L Self-Care dimension}
  \item{\code{activity5L}}{integer EQ-5D-5L Usual activities dimension}
  \item{\code{pain5L}}{integer EQ-5D-5L Pain/discomfort dimension}
  \item{\code{anxiety5L}}{integer EQ-5D-5L Anxiety/depression dimension} 
}
}
\usage{
data(example_data)
}
\description{
A dataset with 3749 participants from six countries containing EQ-5D-3L and EQ-5D-5L data.
}
\keyword{datasets}
