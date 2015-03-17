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

  \item{mat}{a matrix. The distance is calculated by rows.}
  \item{pairwise_fun}{a function which calculates distance between two vectors.}
  \item{...}{pass to \code{\link[stats]{dist}}.}

}
\details{
You can construct any type of distance measurements by defining a pair-wise distance function. The function is implemented by two nested \code{for} loops, thus the efficiency may not be so good.  


}
\value{
A \code{\link[stats]{dist}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
mat = matrix(rnorm(40), nr = 4, ncol = 10)
rownames(mat) = letters[1:4]
colnames(mat) = letters[1:10]

d2 = dist2(mat)
d2 = dist2(mat, pairwise_fun = function(x, y) 1 - cor(x, y))
d2 = dist2(mat, pairwise_fun = function(x, y) {
    l = is.na(x) & is.na(y)
    sqrt(sum((x[l] - y[l])^2))
})
}
