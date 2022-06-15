\name{+.AdditiveUnit}
\alias{add.AdditiveUnit}
\alias{+.AdditiveUnit}
\title{
Horizontally Add Heatmaps or Annotations to a Heatmap List
}
\description{
Horizontally Add Heatmaps or Annotations to a Heatmap List
}
\usage{
\method{+}{AdditiveUnit}(x, y)
}
\arguments{

  \item{x}{A \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}
  \item{y}{A \code{\link{Heatmap-class}} object, a \code{\link{HeatmapAnnotation-class}} object or a \code{\link{HeatmapList-class}} object.}

}
\details{
It is only a helper function. It actually calls
\code{\link{add_heatmap,Heatmap-method}}, \code{\link{add_heatmap,HeatmapList-method}} or
\code{\link{add_heatmap,HeatmapAnnotation-method}} depending on the class of the input
objects.

The \code{\link{HeatmapAnnotation-class}} object to be added should only be row
annotations. Column annotations should be added to the heatmap list by
\code{\link[=pct_v_pct]{\%v\%}}.

\code{x} and \code{y} can also be \code{NULL}.
}
\value{
A \code{\link{HeatmapList-class}} object.
}
\seealso{
\code{\link[=pct_v_pct]{\%v\%}} operator is used for vertical heatmap list.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
