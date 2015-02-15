\name{draw_dimnames-Heatmap-method}
\alias{draw_dimnames,Heatmap-method}
\alias{draw_dimnames}
\title{
Draw row names or column names  


}
\description{
Draw row names or column names  


}
\usage{
\S4method{draw_dimnames}{Heatmap}(object, which = c("row", "column"),
    side = ifelse(which == "row", "right", "bottom"), gp = NULL)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{which}{draw row names or column names.}
  \item{side}{side of dimension names.}
  \item{gp}{graphc parameters for drawing text.}

}
\details{
A viewport is created which contains row names or column names.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
