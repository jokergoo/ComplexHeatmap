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
\S4method{color_mapping_legend}{ColorMapping}(object, ..., plot = TRUE, legend_grid_height = unit(3, "mm"),
    legend_grid_width = unit(3, "mm"), legend_title_gp = gpar(fontsize = 10, fontface = "bold"),
    legend_label_gp = gpar(fontsize = 10))
}
\arguments{

  \item{object}{a \code{\link{ColorMapping}} object.}
  \item{...}{pass to \code{\link[grid]{viewport}}.}
  \item{plot}{whether to plot or just return the size of the legend viewport.}
  \item{legend_grid_height}{height of each legend grid.}
  \item{legend_grid_width}{width of each legend grid.}
  \item{legend_title_gp}{graphic parameter for legend title.}
  \item{legend_label_gp}{graphic parameter for legend label.}

}
\details{
A viewport is created which contains a legend title, legend grids and corresponding labels.  


}
\value{
A \code{\link[grid]{unit}} object which corresponds to the width and height of the legend viewport.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
