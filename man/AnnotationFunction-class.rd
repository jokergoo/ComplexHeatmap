\name{AnnotationFunction-class}
\docType{class}
\alias{AnnotationFunction-class}
\title{
The AnnotationFunction class
}
\description{
The AnnotationFunction class
}
\details{
The heatmap annotation is basically graphics aligned to the columns or heatmap if it is column annotation
or heatmap rows if it is row annotation, while the type of the graphics can be arbitory, e.g.
it can be heatmap-like or points. Here the AnnotationFunction class is designed for creating
complex and flexible annotation graphics. As the main part, it uses a use-defined function
to define the graphics. It also keeps information of the size of the plotting regions of the annotation.
And most importantly, it allows subsetting of the annotation to draw a subset of the graphics, which
is the base for the splitting of the annotations.

See \code{\link{AnnotationFunction}} constructor for details.
}
\examples{
# There is no example
NULL
}
