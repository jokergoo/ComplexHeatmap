\name{color_mapping_legend-ColorMapping-method}
\alias{color_mapping_legend,ColorMapping-method}
\alias{color_mapping_legend}
\title{
Draw legend based on color mapping
}
\description{
Draw legend based on color mapping
}
\usage{
\S4method{color_mapping_legend}{ColorMapping}(object, ...,
    plot = TRUE,
    title = object@name,
    title_gp = gpar(fontsize = 10, fontface = "bold"),
    title_position = c("topleft", "topcenter", "leftcenter", "lefttop"),
    color_bar = c("discrete", "continuous"),
    grid_height = unit(4, "mm"),
    grid_width = unit(4, "mm"),
    grid_border = NULL,
    at = object@levels,
    labels = at,
    labels_gp = gpar(fontsize = 10),
    nrow = NULL,
    ncol = 1,
    legend_height = NULL, legend_width = NULL,
    legend_direction = c("vertical", "horizontal"),
    param = NULL)
}
\arguments{

  \item{object}{a \code{\link{ColorMapping-class}} object.}
  \item{plot}{whether to plot or just return the size of the legend viewport.}
  \item{title}{title of the legend, by default it is the name of the legend}
  \item{title_gp}{graphical parameters for legend title}
  \item{title_position}{position of the title}
  \item{color_bar}{if the mapping is continuous, whether show the legend as discrete color bar or continuous color bar}
  \item{grid_height}{height of each legend grid.}
  \item{grid_width}{width of each legend grid.}
  \item{grid_border}{color for legend grid borders.}
  \item{at}{break values of the legend}
  \item{labels}{labels corresponding to break values}
  \item{labels_gp}{graphcial parameters for legend labels}
  \item{nrow}{if there are too many legend grids, they can be put as an array, this controls number of rows}
  \item{ncol}{if there are too many legend grids, they can be put as an array, this controls number of columns}
  \item{legend_height}{height of the legend, only works when \code{color_bar} is \code{continuous} and \code{direction} is \code{vertical}}
  \item{legend_width}{width of the legend, only works when \code{color_bar} is \code{continuous} and \code{direction} is \code{horizontal}}
  \item{legend_direction}{when \code{color_bar} is \code{continuous}, should the legend be vertical or horizontal?}
  \item{param}{will be parsed if the parameters are specified as a list}
  \item{...}{pass to \code{\link[grid]{viewport}}.}

}
\details{
A viewport is created which contains a legend title, legend grids and corresponding labels.

This function will be improved in the future to support more types of legends.
}
\value{
A \code{\link[grid]{grob}} object which contains the legend
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# discrete color mapping for characters
cm = ColorMapping(name = "test",
    colors = c("blue", "white", "red"),
    levels = c("a", "b", "c"))
grid.newpage()
color_mapping_legend(cm)

# discrete color mapping for numeric values
cm = ColorMapping(name = "test",
    colors = c("blue", "white", "red"),
    levels = c(1, 2, 3))
grid.newpage()
color_mapping_legend(cm)

# continuous color mapping
require(circlize)
cm = ColorMapping(name = "test",
    col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")))
grid.newpage()
color_mapping_legend(cm, title_gp = gpar(fontsize = 16))

}
