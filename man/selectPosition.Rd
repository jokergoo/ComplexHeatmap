\name{selectPosition}
\alias{selectPosition}
\title{
Select a position in the heatmap
}
\description{
Select a position in the heatmap
}
\usage{
selectPosition(ht_list, pos = NULL, mark = TRUE, verbose = TRUE,
    ht_pos = NULL)
}
\arguments{

  \item{ht_list}{A \code{\link{HeatmapList-class}} object returned by \code{\link{draw,Heatmap-method}} or \code{\link{draw,HeatmapList-method}}.}
  \item{mark}{Whether mark the selected position as a point.}
  \item{pos}{If the value is \code{NULL}, it can be selected by click on the heatmap (of cource, the heatmap should be on the interactive graphic device). If it is set, it must be a \code{\link[grid]{unit}} object with length two which corresponds to the x and y position of the point.}
  \item{verbose}{Whether to print messages.}
  \item{ht_pos}{A value returned by \code{\link{ht_pos_on_device}}.}

}
\value{
A \code{\link[S4Vectors:DataFrame-class]{DataFrame}} object with row index and column index corresponding to the selected position.
}
\seealso{
The code for this shiny app is at \url{https://github.com/jokergoo/ComplexHeatmap/blob/master/inst/app/app.R}
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
selectPosition(ht)
}
}
