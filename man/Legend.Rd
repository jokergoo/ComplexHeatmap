\name{Legend}
\alias{Legend}
\title{
Make a Single Legend
}
\description{
Make a Single Legend
}
\usage{
Legend(at, labels = at, col_fun, name = NULL, grob = NULL,
    break_dist = NULL, nrow = NULL, ncol = 1, by_row = FALSE,
    grid_height = unit(4, "mm"),
    grid_width = unit(4, "mm"), tick_length = unit(0.8, "mm"),
    gap = unit(2, "mm"), column_gap = gap, row_gap = unit(0, "mm"),
    labels_gp = gpar(fontsize = 10), labels_rot = 0,
    border = NULL, background = "#EEEEEE",
    type = "grid", graphics = NULL, legend_gp = gpar(),
    pch = 16, size = unit(2, "mm"),
    legend_height = NULL, legend_width = NULL,
    direction = c("vertical", "horizontal"),
    title = "", title_gp = gpar(fontsize = 10, fontface = "bold"),
    title_position = c("topleft", "topcenter", "leftcenter", "lefttop", "leftcenter-rot", "lefttop-rot"),
    title_gap = unit(2, "mm"))
}
\arguments{

  \item{at}{Breaks of the legend. The values can be either numeric or character. If it is not specified, the values of \code{labels} are taken as labels.}
  \item{labels}{Labels corresponding to \code{at}. If it is not specified, the values of \code{at} are taken as labels.}
  \item{col_fun}{A color mapping function which is used to make a continuous legend. Use \code{\link[circlize]{colorRamp2}} to generate the color mapping function. If \code{at} is missing, the breaks recorded in the color mapping function are used for \code{at}.}
  \item{name}{Name of the legend, internally used.}
  \item{grob}{The legend body can be specified by a pre-constructed \code{\link[grid:grid.grob]{grob}} object.}
  \item{break_dist}{A zooming factor to control relative distance of two neighbouring break values.The length of it should be \code{length(at) - 1} or a scalar. }
  \item{nrow}{For legend which is represented as grids, \code{nrow} controls number of rows of the grids if the grids are arranged into multiple rows.}
  \item{ncol}{Similar as \code{nrow}, \code{ncol} controls number of columns of the grids if the grids are arranged into multiple columns. Note at a same time only one of \code{nrow} and \code{ncol} can be specified.}
  \item{by_row}{Are the legend grids arranged by rows or by columns?}
  \item{grid_height}{The height of legend grid. It can also control the height of the continuous legend if it is horizontal.}
  \item{grid_width}{The width of legend grid. It can also control the width of the continuous legend if it is vertical.}
  \item{tick_length}{Length of the ticks on the continuous legends. Value should be a \code{\link[grid]{unit}} object.}
  \item{gap}{If legend grids are put into multiple rows or columns, this controls the gap between neighbouring rows or columns, measured as a \code{\link[grid]{unit}} object.}
  \item{column_gap}{The same as \code{gap}.}
  \item{row_gap}{Space between legend rows.}
  \item{labels_gp}{Graphic parameters for labels.}
  \item{labels_rot}{Text rotation for labels. It should only be used for horizontal continuous legend.}
  \item{border}{Color of legend grid borders. It also works for the ticks in the continuous legend.}
  \item{background}{Background colors for the grids. It is used when points and lines are the legend graphics.}
  \item{type}{Type of legends. The value can be one of \code{grid}, \code{points}, \code{lines} and \code{boxplot}.}
  \item{graphics}{Self-defined graphics for legends. The value should be a list of functions. Each function should accept four argumets: \code{x} and \code{y}: positions of the legend grid (center point), \code{w} and \code{h}: width and height of the legend grid.}
  \item{legend_gp}{Graphic parameters for the legend grids. You should control the filled color of the legend grids by \code{gpar(fill = ...)}.}
  \item{pch}{Type of points if points are used as legend. Note you can use single-letter as pch, e.g. \code{pch = 'A'}. There are three additional integers that are valid for \code{pch}: 26 and 27 for single diagonal lines and 28 for double diagonal lines.}
  \item{size}{Size of points.}
  \item{legend_height}{Height of the whole legend body. It is only used for vertical continous legend.}
  \item{legend_width}{Width of the whole legend body. It is only used for horizontal continous legend.}
  \item{direction}{Direction of the legend, vertical or horizontal?}
  \item{title}{Title of the legend.}
  \item{title_gp}{Graphic parameters of the title.}
  \item{title_position}{Position of title relative to the legend. \code{topleft}, \code{topcenter}, \code{leftcenter-rot} and \code{lefttop-rot} are only for vertical legend and \code{leftcenter}, \code{lefttop} are only for  horizontal legend.}
  \item{title_gap}{Gap between title and the legend body.}

}
\details{
Most of the argument can also be set in \code{heatmap_legend_param} argument in \code{\link{Heatmap}} or \code{annotation_legend_param}
argument in \code{\link{HeatmapAnnotation}} to configure legend styles for heatmap and annotations.
}
\seealso{
\code{\link{packLegend}} packs multiple legends into one \code{\link{Legends-class}} object.

See examples of configuring legends: \url{https://jokergoo.github.io/ComplexHeatmap-reference/book/legends.html}
}
\value{
A \code{\link{Legends-class}} object.
}
\examples{
lgd = Legend(labels = month.name[1:6], title = "foo", legend_gp = gpar(fill = 1:6))
draw(lgd, test = "add labels and title")

require(circlize)
col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
lgd = Legend(col_fun = col_fun, title = "foo")
draw(lgd, test = "only col_fun")

col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.15, 0.5, 0.9, 0.95, 1))
draw(lgd, test = "unequal interval breaks")
}
