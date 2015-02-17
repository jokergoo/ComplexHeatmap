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
\S4method{draw_dimnames}{Heatmap}(object,
    which = c("row", "column"), k = 1, ...)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}
  \item{side}{side of dimension names.}
  \item{k}{which matrix in the matrix list}
  \item{gp}{graphc parameters for drawing text.}
  \item{...}{pass to \code{\link[grid]{viewport}}.}

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
