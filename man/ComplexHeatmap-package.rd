\name{ComplexHeatmap-package}
\docType{package}
\alias{ComplexHeatmap-package}
\title{
Making complex heatmap
}
\description{
Making complex heatmap
}
\details{
This package aims to provide a simple and flexible way to arrange
multiple heatmaps as well as self-defining annotation graphics.

The package is implemented in an object-oriented way. 
Components of heatmap lists are abstracted into several classes.

\itemize{
  \item \code{\link{Heatmap-class}}: a single heatmap containing heatmap body, row/column names, titles, dendrograms and column annotations.
  \item \code{\link{HeatmapList-class}}: a list of heatmaps and row annotations.
  \item \code{\link{HeatmapAnnotation-class}}: a list of row annotations or column annotations.
}

There are also several internal classes:

\itemize{
  \item \code{\link{SingleAnnotation-class}}: a single row annotation or column annotation.
  \item \code{\link{ColorMapping-class}}: mapping from values to colors.
}

For plotting one single heatmap, please go to the documentation page of \code{\link{Heatmap}}.
For plotting multiple heatmaps, please go to \code{\link{HeatmapList-class}} and \code{+.AdditiveUnit}.

The vignette provides detailed explanation of how to use this package.
}
\examples{
# There is no example
NULL

}
