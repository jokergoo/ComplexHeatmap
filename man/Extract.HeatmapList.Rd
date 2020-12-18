\name{[.HeatmapList}
\alias{Extract.HeatmapList}
\alias{[.HeatmapList}
\title{
Subset a HeatmapList object
}
\description{
Subset a HeatmapList object
}
\usage{
\method{[}{HeatmapList}(x, i, j)
}
\arguments{

  \item{x}{A \code{\link{HeatmapList-class}} object}
  \item{i}{row indices}
  \item{j}{column indices}

}
\details{
If the heatmap list is horizontal, \code{i} is the row indices and \code{j} corresponds to heatmap names and single annotation names.
and if the heatlist is vertical, \code{i} corresponds to heatmap/annotation names and \code{j} is the column indices.
}
\examples{
ht_list = Heatmap(matrix(rnorm(100), 10), name = "rnorm") +
  rowAnnotation(foo = 1:10, bar = anno_points(10:1)) + 
  Heatmap(matrix(runif(100), 10), name = "runif")
summary(ht_list[1:5, ])
summary(ht_list[1:5, 1])
summary(ht_list[1:5, "rnorm"])
summary(ht_list[1:5, c("rnorm", "foo")])

ht_list = Heatmap(matrix(rnorm(100), 10), name = "rnorm") \%v\%
  columnAnnotation(foo = 1:10, bar = anno_points(10:1)) \%v\%
  Heatmap(matrix(runif(100), 10), name = "runif")
summary(ht_list[, 1:5])
summary(ht_list[1, 1:5])
summary(ht_list["rnorm", 1:5])
summary(ht_list[c("rnorm", "foo"), 1:5])
}
