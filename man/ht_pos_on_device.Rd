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
It returns a \code{\link[S4Vectors:DataFrame-class]{DataFrame}} object of the positions of every heatmap slice.
}
\examples{
if(interactive()) {
m = matrix(rnorm(100), 10)
ht = Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
pos = ht_pos_on_device(ht)

ComplexHeatmap:::redraw_ht_vp(pos)
}
}
