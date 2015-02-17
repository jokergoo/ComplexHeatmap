\name{draw_title-HeatmapList-method}
\alias{draw_title,HeatmapList-method}
\title{
Draw heatmap list title  


}
\description{
Draw heatmap list title  


}
\usage{
\S4method{draw_title}{HeatmapList}(object, title, which = c("row", "column"),
    side = ifelse(which == "row", "right", "bottom"), gp = NULL)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList}} object}
  \item{title}{title}
  \item{which}{the title should be plotted on rows or columns.}
  \item{side}{side of heatmap title.}
  \item{gp}{graphic paramter for drawing text.}

}
\details{
A viewport is created which contains heatmap list title.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
