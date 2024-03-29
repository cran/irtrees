% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dendrify.R
\name{tolong}
\alias{tolong}
\title{Convert a wide-format matrix to long format}
\usage{
tolong(mat)
}
\arguments{
\item{mat}{an integer IRT response matrix (i.e. a wide format)}
}
\value{
a long-format data frame
}
\description{
Convert an IRT response matrix in the wide format to a data frame
in the long format. In the wide format each row corresponds to a
respondent and each column to an item.
}
