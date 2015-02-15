\name{draw_hclust-Heatmap-method}
\alias{draw_hclust,Heatmap-method}
\title{
plot the dendrogram on rows or columns  


}
\description{
plot the dendrogram on rows or columns  


}
\usage{
\S4method{draw_hclust}{Heatmap}(object, which = c("row", "column"),
    side = ifelse(which == "row", "left", "top"), gp = NULL)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object}
  \item{which}{the dendrogram should be plotted on rows or columns}
  \item{side}{side of the dendrogram}
  \item{gp}{graphic parameters for drawing lines}

}
\details{
the dendrogram 100% covers the viewport  


}
\value{
this function returns no value  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\alias{draw_hclust}
