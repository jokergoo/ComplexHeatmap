\name{ht_pos_on_device}
\alias{ht_pos_on_device}
\title{
Get the heatmap positions on the graphic device
}
\description{
Get the heatmap positions on the graphic device
}
\usage{
ht_pos_on_device(ht_list, unit = "inch", valueOnly = FALSE)
}
\arguments{

  \item{ht_list}{A \code{\link{HeatmapList-class}} object returned by \code{\link{draw,Heatmap-method}} or \code{\link{draw,HeatmapList-method}}.}
  \item{unit}{The unit.}
  \item{valueOnly}{Whether only return the numeric values.}

}
\value{
It returns a deep list of the positions of the bottom left and top right
of every heatmap slice.

The format is (assme the returned object is called \code{pos}):

\itemize{
  \item \code{pos[[ heatmap_name ]][[ slice_name ]][[ x ]]}: left and right of the selected sub-heatmap.
  \item \code{pos[[ heatmap_name ]][[ slice_name ]][[ y ]]}: bottom and top of the selected sub-heatmap.
}
}
\examples{
\dontrun{
# it is runable if you are under an interactive graphic device
m = matrix(rnorm(100), 10)
ht = Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
pos = ht_pos_on_device(ht)

ds = dev.size()
dev.new(width = ds[1], height = ds[2])
grid.newpage()
for(ht_name in names(pos)) {
	for(vp_name in names(pos[[ht_name]])) {
		x = pos[[ht_name]][[vp_name]]$x
		y = pos[[ht_name]][[vp_name]]$y
		pushViewport(viewport(x = x[1], y = y[1], 
			width = x[2] - x[1], height = y[2] - y[1],
			just = c("left", "bottom")))
		grid.rect()
		popViewport()
	}
}
}
}
