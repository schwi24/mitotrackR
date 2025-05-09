% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/track_branches.R
\name{fast_left_join}
\alias{fast_left_join}
\title{Fast left join for large tables}
\usage{
fast_left_join(left, right, by)
}
\arguments{
\item{left}{The left-side table to join.  Any class inheriting from
\code{data.frame} is acceptable.}

\item{right}{The right-side table to join.  Any class inheriting from
\code{data.frame} is acceptable.}

\item{by}{Character vector of column names to join by.}
}
\value{
The left join of \code{left} and \code{right}.  It will be returned
as a \code{tbl_df}, irrespective of the type of the inputs.
}
\description{
This function was taken from https://rdrr.io/github/JGCRI/gcamdata/src/R/pipeline-helpers.R
}
