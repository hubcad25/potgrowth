% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scales_and_party_positions.R
\name{retrieve_rcis}
\alias{retrieve_rcis}
\title{Retrieve Relative Confidence Indices (RCIs) for Parties}
\usage{
retrieve_rcis(
  data,
  rci_columns_prefix = "irc_",
  parties = potgrowth::qc_parties
)
}
\arguments{
\item{data}{A dataframe containing the dataset from which RCIs are to be retrieved.}

\item{rci_columns_prefix}{A string specifying the prefix of the columns in `data` that contain RCI values. Defaults to "irc_".}

\item{parties}{A vector of strings specifying the parties for which RCIs are to be retrieved. This parameter defaults to `potgrowth::qc_parties`, assuming `potgrowth` package contains a list of Quebec parties.}
}
\value{
A matrix containing the RCI values for the specified parties.
}
\description{
This function extracts the Relative Confidence Indices (RCIs) for specified parties from a dataset. The RCI measures the confidence level relative to other indices.
}
\examples{
rcis <- retrieve_rcis(my_data, "irc_", c("party1", "party2"))
}
