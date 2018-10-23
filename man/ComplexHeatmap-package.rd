\name{ComplexHeatmap-package}
\docType{package}
\alias{ComplexHeatmap-package}
\title{
Make complex heatmaps
}
\description{
Make complex heatmaps
}
\details{
This package aims to provide a simple and flexible way to arrange
multiple heatmaps as well as flexible annotation graphics.

The package is implemented in an object-oriented way. 
The heatmap lists are abstracted into several classes.

\itemize{
  \item \code{\link{Heatmap-class}}: a single heatmap containing heatmap body, row/column names, titles, dendrograms and annotations.
  \item \code{\link{HeatmapList-class}}: a list of heatmaps and annotations.
  \item \code{\link{HeatmapAnnotation-class}}: a list of row/column annotations.
}

There are also several internal classes:

\itemize{
  \item \code{\link{SingleAnnotation-class}}: a single row annotation or column annotation.
  \item \code{\link{ColorMapping-class}}: mapping from values to colors.
  \item \code{\link{AnnotationFunction-class}}: construct an annotation function which allows subsetting.
}

Following two high-level functions take use of functionality of complex heatmaps:

\itemize{
  \item \code{\link{oncoPrint}}: oncoPrint plot which visualize genomic alterations in a set of genes.
  \item \code{\link{densityHeatmap}}: use heatmaps to visualize density distributions.
}

The complete reference of ComplexHeatmap package is available at \url{http://jokergoo.github.io/ComplexHeatmap-reference/book.}
}
\examples{
# There is no example
NULL

}
