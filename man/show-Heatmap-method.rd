\name{show-Heatmap-method}
\alias{show,Heatmap-method}
\title{
Draw the single heatmap with default parameters
}
\description{
Draw the single heatmap with default parameters
}
\usage{
\S4method{show}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap-class}} object.}

}
\details{
Actually it calls \code{\link{draw,Heatmap-method}}, but only with default parameters. If users want to customize the heatmap,
they can pass parameters directly to \code{\link{draw,Heatmap-method}}.
}
\value{
This function returns no value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(80, 2), 8, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:12]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
ht
draw(ht, heatmap_legend_side = "left")

}
