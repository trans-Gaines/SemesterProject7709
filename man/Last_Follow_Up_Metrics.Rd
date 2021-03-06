% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PrepareRedcapData.R
\name{Last_Follow_Up_Metrics}
\alias{Last_Follow_Up_Metrics}
\title{Take your redcap outputs and prepare them for analysis}
\usage{
Last_Follow_Up_Metrics(dataset, lengthoffollowuprepeating, personidentifier)
}
\arguments{
\item{dataset}{Your prepared dataset from redcap}

\item{lengthoffollowuprepeating}{The repeating instrument parameter from r that gives you POD of follow up.}

\item{personidentifierg}{The record/person level parameter from r that gives you the unique person}
}
\value{
The data set limited to the last follow up repeatable instrument.
}
\description{
Take your redcap outputs and prepare them for analysis
}
\examples{
examples forthcoming
}
