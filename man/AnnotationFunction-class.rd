\name{AnnotationFunction-class}
\docType{class}
\alias{AnnotationFunction-class}
\title{
The AnnotationFunction Class
}
\description{
The AnnotationFunction Class
}
\details{
The heatmap annotation is basically graphics aligned to the heatmap columns or rows.
There is no restriction for the graphic types, e.g. it can be heatmap-like
annotation or points. Here the AnnotationFunction class is designed for
creating complex and flexible annotation graphics. As the main part of the class, it uses
a user-defined function to define the graphics. It also keeps information of
the size of the plotting regions of the annotation. And most importantly, it
allows subsetting to the annotation to draw a subset of the graphics, which
is the base for the splitting of the annotations.

See \code{\link{AnnotationFunction}} constructor for details.
}
\examples{
# There is no example
NULL

}
