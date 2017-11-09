\name{Legend}
\alias{Legend}
\title{
Making legend grobs
}
\description{
Making legend grobs
}
\usage{
Legend(at, labels = at, nrow = NULL, ncol = 1, col_fun, by_row = FALSE,
    grid_height = unit(4, "mm"), grid_width = unit(4, "mm"), gap = unit(2, "mm"),
    labels_gp = gpar(fontsize = 10),
    border = NULL, background = "#EEEEEE",
    type = "grid", legend_gp = gpar(),
    pch = 16, size = unit(2, "mm"),
    legend_height = NULL, legend_width = NULL,
    direction = c("vertical", "horizontal"),
    title = "", title_gp = gpar(fontsize = 10, fontface = "bold"),
    title_position = c("topleft", "topcenter", "leftcenter", "lefttop"))
}
\arguments{

  \item{at}{breaks, can be wither numeric or character}
  \item{labels}{labels corresponding to \code{at}}
  \item{nrow}{if there are too many legends, they can be positioned in an array, this controls number of rows}
  \item{ncol}{if there are too many legends, they can be positioned in an array, this controls number of columns. At a same time only one of \code{nrow} and \code{ncol} can be specified.}
  \item{col_fun}{a color mapping function which is used to make a continuous color bar}
  \item{by_row}{when there are multiple columns for legends, whether to arrange them by rows.}
  \item{grid_height}{height of legend grid}
  \item{grid_width}{width of legend grid}
  \item{gap}{when legends are put in multiple columns, this is the gap between neighbouring columns, measured as a \code{\link[grid]{unit}} object}
  \item{labels_gp}{graphic parameters for labels}
  \item{border}{color of legend borders, also for the ticks in the continuous legend}
  \item{background}{background colors}
  \item{type}{type of legends, can be \code{grid}, \code{points} and \code{lines}}
  \item{legend_gp}{graphic parameters for the legend}
  \item{pch}{type of points}
  \item{size}{size of points}
  \item{legend_height}{height of the whole legend, used when \code{col_fun} is specified and \code{direction} is set to \code{vertical}}
  \item{legend_width}{width of the whole legend, used when \code{col_fun} is specified  and \code{direction} is set to \code{horizontal}}
  \item{direction}{direction of the continuous or discrete legend}
  \item{title}{title of the legend}
  \item{title_gp}{graphic parameters of title}
  \item{title_position}{position of title according to the legend}

}
\seealso{
\code{\link{packLegend}} packs multiple legends into one \code{\link[grid]{grob}} object
}
\value{
A \code{\link[grid]{grob}} object
}
\examples{
lgd = Legend(title = "discrete", at = 1:4, labels = letters[1:4], 
	legend_gp = gpar(fill = 2:5))
grid.newpage()
grid.draw(lgd)

require(circlize)
col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
lgd = Legend(title = "continuous", at = seq(-1, 1, by = 0.5), col_fun = col_fun)
grid.newpage()
grid.draw(lgd)

lgd = Legend(title = "continuous", at = seq(-1, 1, by = 0.5), col_fun = col_fun,
	direction = "horizontal")
grid.newpage()
grid.draw(lgd)

lgd = Legend(title = "discrete", at = 1:10, labels = letters[1:10], 
	ncol = 4, by_row = TRUE, legend_gp = gpar(fill = rand_color(10)))
grid.newpage()
grid.draw(lgd)

lgd = Legend(title = "lty", at = 1:3, labels = 1:3, type = "lines",
	legend_gp = gpar(lty = 1:3))
grid.newpage()
grid.draw(lgd)
}
