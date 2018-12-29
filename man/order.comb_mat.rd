\name{order.comb_mat}
\alias{order.comb_mat}
\title{
Order of the Combination Sets
}
\description{
Order of the Combination Sets
}
\usage{
order.comb_mat(m, decreasing = TRUE, on = "comb_set")
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}
  \item{on}{On sets or on combination sets?}
  \item{decreasing}{Whether the ordering is applied decreasingly.}

}
\details{
It first sorts by the degree of the combination sets then
by the combination matrix.
}
\examples{
# There is no example
NULL

}
