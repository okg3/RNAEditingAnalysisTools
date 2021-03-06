% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetEditedReads.R
\name{GetEditedReads}
\alias{GetEditedReads}
\title{Calculate number of edited reads from frequency and coverage}
\usage{
GetEditedReads(x)
}
\arguments{
\item{x}{An RNA editing data object which includes Frequency matrix and
Coverage-q25 matrix at minimum}
}
\value{
A matrix with total number of edited reads for each edited site by
  individual
}
\description{
\code{GetEditedReads} calculates the number of edited reads using the
Frequency and Coverage-q25 matrices from the RNA Editing data object
}
\details{
The input should be a list object which includes matrices named "Frequency"
and "Coverage-q25".
}
\examples{
\dontrun{
head(GetEditedReads(RNAEdData))
}
}
