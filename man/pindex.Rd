\name{pindex}
\alias{pindex}
\title{
Get Values in a Matrix by Pair-wise Indices
}
\description{
Get Values in a Matrix by Pair-wise Indices
}
\usage{
pindex(m, i, j)
}
\arguments{

  \item{m}{A matrix or a 3-dimension array.}
  \item{i}{Row indices or the indices in the first dimension.}
  \item{j}{Column indicies or the indices in the second dimension.}

}
\value{
If \code{m} is a matrix, the value returned is a vector \code{c(m[i1, j1], m[i2, j2], ...)}`.

If \code{m} is an array, the value returned is a matrix \code{rbind(m[i1, j1, ], m[i2, j2, ], ...)}`.
}
\examples{
m = matrix(rnorm(100), 10)
m2 = m[m > 0]
ind = do.call("rbind", lapply(1:10, function(ci) {
    i = which(m[, ci] > 0)
    cbind(i = i, j = rep(ci, length(i)))
}))
pindex(m, ind[, 1], ind[, 2])
identical(pindex(m, ind[, 1], ind[, 2]), m[m > 0])

# 3d array
arr = array(1:27, dim = c(3, 3, 3))
pindex(arr, 1:2, 2:3)
identical(pindex(arr, 1:2, 2:3),
   rbind(arr[1, 2, ], arr[2, 3, ]))
}
