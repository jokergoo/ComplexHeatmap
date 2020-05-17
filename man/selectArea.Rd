\name{selectArea}
\alias{selectArea}
\title{
Select an area in the heatmap
}
\description{
Select an area in the heatmap
}
\usage{
selectArea(ht_list, pos1 = NULL, pos2 = NULL, mark = TRUE, verbose = TRUE,
    ht_pos = NULL)
}
\arguments{

  \item{ht_list}{A \code{\link{HeatmapList-class}} object returned by \code{\link{draw,Heatmap-method}} or \code{\link{draw,HeatmapList-method}}.}
  \item{mark}{Whether mark the selected area as a rectangle.}
  \item{pos1}{If the value is \code{NULL}, it can be selected by click on the heatmap (of cource, the heatmap should be on the interactive graphic device). If it is set, it must be a \code{\link[grid]{unit}} object with length two which corresponds to the x and y position of the point.}
  \item{pos2}{Another point as \code{pos1}, together with \code{pos1} defines the selected region.}
  \item{verbose}{Whether to print messages.}
  \item{ht_pos}{A value returned by \code{\link{ht_pos_on_device}}.}

}
\details{
The regions can be selected interactively or manually by setting \code{pos1} and \code{pos2}.
}
\value{
A \code{\link[S4Vectors:DataFrame-class]{DataFrame}} object with row indices and column indices corresponding to the selected region.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
if(interactive()) {
	m = matrix(rnorm(100), 10)
	rownames(m) = 1:10
	colnames(m) = 1:10

ht = Heatmap(m)
	ht = draw(ht)
	selectArea(ht)

set.seed(123)
	ht = Heatmap(m, row_km = 2, column_km = 2)
	ht = draw(ht)
	selectArea(ht)
}
}
