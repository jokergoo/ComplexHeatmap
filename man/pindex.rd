\name{pindex}
\alias{pindex}
\title{
Get values in a matrix by pair-wise indices
}
\description{
Get values in a matrix by pair-wise indices
}
\usage{
pindex(m, i, j)
}
\arguments{

  \item{m}{a matrix or a 3d array}
  \item{i}{row indices}
  \item{j}{column indicies}

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
