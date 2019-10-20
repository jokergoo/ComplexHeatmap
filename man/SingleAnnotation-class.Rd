\name{SingleAnnotation-class}
\docType{class}
\alias{SingleAnnotation-class}
\title{
Class for a Single Annotation
}
\description{
Class for a Single Annotation
}
\details{
The \code{\link{SingleAnnotation-class}} is used for storing data for a single annotation and provides
methods for drawing annotation graphics.
}
\section{Methods}{
The \code{\link{SingleAnnotation-class}} provides following methods:

\itemize{
  \item \code{\link{SingleAnnotation}}: constructor method
  \item \code{\link{draw,SingleAnnotation-method}}: draw the single annotation.
}}
\seealso{
The \code{\link{SingleAnnotation-class}} is always used internally. The public \code{\link{HeatmapAnnotation-class}}
contains a list of \code{\link{SingleAnnotation-class}} objects and is used to add annotation graphics on heatmaps.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
