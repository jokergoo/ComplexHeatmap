\name{dist2}
\alias{dist2}
\title{
Calculate pairwise distance from a matrix
}
\description{
Calculate pairwise distance from a matrix
}
\usage{
dist2(mat, pairwise_fun = function(x, y) sqrt(sum((x - y)^2)), ...)
}
\arguments{

  \item{mat}{a matrix. The distance is calculated by rows.}
  \item{pairwise_fun}{a function which calculates distance between two vectors.}
  \item{...}{pass to \code{\link[stats]{as.dist}}.}

}
\details{
You can construct any type of distance measurements by defining a pair-wise distance function.
The function is implemented by two nested \code{for} loops, so the efficiency may not be so good.
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
# distance only calculated within 10 and 90 quantile of each vector
d2 = dist2(mat, pairwise_fun = function(x, y) {
	q1 = quantile(x, c(0.1, 0.9))
	q2 = quantile(y, c(0.1, 0.9))
    l = x > q1[1] & x < q1[2] & y > q2[1] & y < q2[2]
    sqrt(sum((x[l] - y[l])^2))
})

}
