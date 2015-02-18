\name{dist2}
\alias{dist2}
\title{
Calculate distance from a matrix  


}
\description{
Calculate distance from a matrix  


}
\usage{
dist2(mat, pairwise_fun = function(x, y) sqrt(sum((x - y)^2)), ...)
}
\arguments{

  \item{mat}{a matrix. The distance is calculated by rows.}
  \item{pairwise_fun}{a function which calculates distance between two vectors.}
  \item{...}{pass to \code{\link[stats]{dist}}.}

}
\details{
You can construct any type of distance measurements by defining a pair-wise distance function.  


}
\value{
A \code{\link[stats]{dist}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
