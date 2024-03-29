% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales_and_party_positions.R
\name{compute_scale_scores}
\alias{compute_scale_scores}
\title{Compute Scale Score from Factor Analysis Loadings}
\usage{
compute_scale_scores(factanal_object, survey_data, scale_order)
}
\arguments{
\item{factanal_object}{An object of class 'factanal' representing the result of a factor analysis.}

\item{survey_data}{A data frame containing survey data where column names match the variables used in the factor analysis.}

\item{scale_order}{A numeric or character vector indicating the columns of the loadings to be used.}
}
\value{
A numeric vector of scale scores for the survey data based on the specified factor analysis loadings.
}
\description{
This function computes scale scores based on factor analysis loadings and survey data.
It takes a factor analysis object, survey data, and the order of the scale as inputs.
It returns the computed scale score for the specified scale.
}
\examples{
# Assume factanal_object is the result of a factor analysis and df is your survey data
# compute_scale_score(factanal_object, df, 1) # For the first factor
}
