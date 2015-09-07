\name{SingleAnnotation-class}
\docType{class}
\alias{SingleAnnotation-class}
\title{
Class for a single annotation
}
\description{
Class for a single annotation
}
\details{
A complex heatmap always has more than one annotations on rows and columns. Here
the \code{\link{SingleAnnotation-class}} defines the basic unit of annotations.
The most simple annotation is one row or one column grids in which different colors
represent different classes of the data. The annotation can also be more complex
graphics, such as a boxplot that shows data distribution in corresponding row or column.

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
# for examples, please go to `SingleAnnotation` method page
NULL

}
