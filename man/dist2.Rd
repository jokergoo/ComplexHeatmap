\name{dist2}
\alias{dist2}
\title{
Calculate Pairwise Distance from a Matrix
}
\description{
Calculate Pairwise Distance from a Matrix
}
\usage{
dist2(x, pairwise_fun = function(x, y) sqrt(sum((x - y)^2)), ...)
}
\arguments{

  \item{x}{A matrix or a list. If it is a matrix, the distance is calculated by rows.}
  \item{pairwise_fun}{A function which calculates distance between two vectors.}
  \item{...}{Pass to \code{\link[stats:dist]{as.dist}}.}

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
lt = lapply(1:10, function(i) {
    sample(letters, sample(6:10, 1))
})
dist2(lt, function(x, y) {
    length(intersect(x, y))/length(union(x, y))
})
}
