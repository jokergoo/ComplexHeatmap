\name{ht_global_opt}
\alias{ht_global_opt}
\title{
Global Options for Heatmaps
}
\description{
Global Options for Heatmaps
}
\usage{
ht_global_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{Options.}
  \item{RESET}{Reset all the option values.}
  \item{READ.ONLY}{\code{TRUE} means only to return read-only values, \code{FALSE} means only to return non-read-only values, \code{NULL} means to return both.}
  \item{LOCAL}{Wwitch to local mode.}
  \item{ADD}{Add new options.}

}
\details{
This function is deprecated. Please use \code{\link{ht_opt}} instead. However, changes by this function
will also be sychronized in \code{\link{ht_opt}}.
}
\examples{
# There is no example
NULL

}
