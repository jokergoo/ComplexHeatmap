\name{[.Heatmap}
\alias{[.Heatmap}
\alias{Extract.Heatmap}
\title{
Subset a Heatmap
}
\description{
Subset a Heatmap
}
\usage{
\method{[}{Heatmap}(x, i, j)
}
\arguments{

  \item{x}{A \code{\link{Heatmap-class}} object.}
  \item{i}{Row indices}
  \item{j}{Column indices}

}
\examples{
m = matrix(rnorm(100), nrow = 10)
rownames(m) = letters[1:10]
colnames(m) = LETTERS[1:10]
ht = Heatmap(m)
ht[1:5, ]
ht[1:5]
ht[, 1:5]
ht[1:5, 1:5]
}
