\name{dist2}
\alias{dist2}
\title{
calculate distance from a matrix  


}
\description{
calculate distance from a matrix  


}
\usage{
dist2(mat, pairwise_fun = function(x, y) sqrt(sum((x - y)^2)), ...)
}
\arguments{

  \item{mat}{a matrix}
  \item{pairwise_fun}{a function which calculates distance between two vectors}
  \item{...}{pass to \code{\link[stats]{dist}}}

}
\details{
self-define distance metric  


}
\value{
a \code{\link[stats]{dist}} object  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
