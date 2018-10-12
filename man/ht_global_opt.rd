\name{ht_global_opt}
\alias{ht_global_opt}
\title{
Global graphic options for heatmaps
}
\description{
Global graphic options for heatmaps
}
\usage{
ht_global_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{options, see 'details' section}
  \item{RESET}{reset all the option values}
  \item{READ.ONLY}{\code{TRUE} means only to return read-only values, \code{FALSE} means only to return non-read-only values, \code{NULL} means to return both.}
  \item{LOCAL}{switch local mode}
  \item{ADD}{add new options}

}
\details{
This function is deprecated. Please use \code{\link{ht_opt}} instead. However, changes by this function
will also be sychronized in \code{\link{ht_opt}}.
}
\examples{
# There is no example
NULL

}
