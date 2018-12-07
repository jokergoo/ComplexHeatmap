\name{draw-SingleAnnotation-method}
\alias{draw,SingleAnnotation-method}
\title{
Draw the Single Annotation
}
\description{
Draw the Single Annotation
}
\usage{
\S4method{draw}{SingleAnnotation}(object, index, k = 1, n = 1, test = FALSE,
    anno_mark_param = list())
}
\arguments{

  \item{object}{A \code{\link{SingleAnnotation-class}} object.}
  \item{index}{A vector of indices.}
  \item{k}{The index of the slice.}
  \item{n}{Total number of slices. \code{k} and \code{n} are used to adjust annotation names. E.g. if \code{k} is 2 and \code{n} is 3, the annotation names are not drawn.}
  \item{test}{Is it in test mode? The value can be logical or a text which is plotted as the title of plot.}
  \item{anno_mark_param}{It contains specific parameters for drawing \code{\link{anno_mark}}.}

}
\value{
No value is returned.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
