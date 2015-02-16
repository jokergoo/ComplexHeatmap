\name{draw_title-Heatmap-method}
\alias{draw_title,Heatmap-method}
\title{
Draw heatmap title  


}
\description{
Draw heatmap title  


}
\usage{
\S4method{draw_title}{Heatmap}(object, title, which = c("row", "column"),
    side = ifelse(which == "row", "right", "bottom"), gp = NULL)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{title}{title.}
  \item{which}{the title should be plotted on rows or columns.}
  \item{side}{side of heatmap title.}
  \item{gp}{graphic paramter for drawing text.}

}
\details{
A viewport is created which contains heatmap title.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
