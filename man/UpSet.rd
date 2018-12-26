\name{UpSet}
\alias{UpSet}
\title{
Make the UpSet plot
}
\description{
Make the UpSet plot
}
\usage{
UpSet(m, set_order = order(set_size(m), decreasing = TRUE),
    comb_order = order(comb_size(m), decreasing = TRUE), ...)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}. The matrix can be transposed to switch the position of sets and combination sets.}
  \item{set_order}{The order of sets.}
  \item{comb_order}{The order of combination sets.}
  \item{...}{Other arguments passed to \code{\link{Heatmap}}.}

}
\examples{
# There is no example
NULL

}
