\name{draw_heatmap_body-Heatmap-method}
\alias{draw_heatmap_body,Heatmap-method}
\alias{draw_heatmap_body}
\title{
Draw Heatmap Body
}
\description{
Draw Heatmap Body
}
\usage{
\S4method{draw_heatmap_body}{Heatmap}(object, kr = 1, kc = 1, ...)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{kr}{Row slice index.}
  \item{kc}{Column slice index.}
  \item{...}{Pass to \code{\link[grid]{viewport}} which includes the slice of heatmap body.}

}
\details{
A viewport is created which contains subset rows and columns of the heatmap.

This function is only for internal use.
}
\value{
This function returns no value.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
