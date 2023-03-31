\name{color_mapping_legend-ColorMapping-method}
\alias{color_mapping_legend,ColorMapping-method}
\alias{color_mapping_legend}
\title{
Draw Legend Based on Color Mapping
}
\description{
Draw Legend Based on Color Mapping
}
\usage{
\S4method{color_mapping_legend}{ColorMapping}(object,
    plot = TRUE, ...,
    
    color_bar = object@type,
    
    title = object@name,
    title_gp = gpar(fontsize = 10, fontface = "bold"),
    title_position = "topleft",
    grid_height = unit(4, "mm"),
    grid_width = unit(4, "mm"),
    tick_length = unit(0.8, "mm"),
    border = NULL,
    at = object@levels,
    labels = at,
    labels_gp = gpar(fontsize = 10),
    labels_rot = 0,
    nrow = NULL,
    ncol = 1,
    by_row = FALSE,
    legend_gp = gpar(),
    legend_height = NULL,
    legend_width = NULL,
    legend_direction = c("vertical", "horizontal"),
    break_dist = NULL,
    
    graphics = NULL,
    param = NULL)
}
\arguments{

  \item{object}{A \code{\link{ColorMapping-class}} object.}
  \item{plot}{Whether to plot or just return the legend object?}
  \item{...}{Pass to \code{\link{draw,Legends-method}}.}
  \item{color_bar}{"continous" or "discrete". It controls whether to show the discrete legend for the continuous color mapping.}
  \item{title}{Title of the legend, by default it is the name of the legend.}
  \item{title_gp}{Graphical parameters for legend title.}
  \item{title_position}{Position of the title. See \code{\link{Legend}} for all possible values.}
  \item{grid_height}{Height of each legend grid. Pass to \code{\link{Legend}}.}
  \item{grid_width}{Width of each legend grid. Pass to \code{\link{Legend}}.}
  \item{tick_length}{Length of the ticks on the continuous legends. Value should be a \code{\link[grid]{unit}} object.}
  \item{border}{Color for legend grid borders. Pass to \code{\link{Legend}}.}
  \item{at}{Break values of the legend. By default it is the levels in the \code{\link{ColorMapping-class}} object.}
  \item{labels}{Labels corresponding to break values.}
  \item{labels_gp}{Graphcial parameters for legend labels.}
  \item{labels_rot}{Rotation of labels.}
  \item{nrow}{Pass to \code{\link{Legend}}. It controls the layout of legend grids if they are arranged in multiple rows or columns.}
  \item{ncol}{Pass to \code{\link{Legend}}. It controls the layout of legend grids if they are arranged in multiple rows or columns.}
  \item{by_row}{Pass to \code{\link{Legend}}. It controls the order of legend grids if they are arranged in multiple rows or columns.}
  \item{legend_gp}{Graphic parameters for legend.}
  \item{legend_height}{Height of the legend body. It only works when \code{color_bar} is \code{continuous} and \code{direction} is \code{vertical}. Pass to \code{\link{Legend}}.}
  \item{legend_width}{Width of the legend body. It only works when \code{color_bar} is \code{continuous} and \code{direction} is \code{horizontal}. Pass to \code{\link{Legend}}.}
  \item{legend_direction}{When \code{color_bar} is \code{continuous}, whether the legend is vertical or horizontal? Pass to \code{\link{Legend}}.}
  \item{break_dist}{A zooming factor to control relative distance of two neighbouring break values.The length of it should be \code{length(at) - 1} or a scalar. }
  \item{graphics}{Internally used.}
  \item{param}{All the legend-related parameters can be specified as a single list.}

}
\details{
The legend is constructed by \code{\link{Legend}}.
}
\value{
A \code{\link{Legends-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
