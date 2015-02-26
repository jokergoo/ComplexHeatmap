\name{initialize-HeatmapAnnotation-method}
\alias{initialize,HeatmapAnnotation-method}
\title{
Constructor method for HeatmapAnnotation class  


}
\description{
Constructor method for HeatmapAnnotation class  


}
\usage{
\S4method{initialize}{HeatmapAnnotation}(.Object, df, name, col, show_legend, ..., which = c("column", "row"),
    annotation_height = 1, annotation_width = 1, height = NULL, width = NULL)
}
\arguments{

  \item{.Object}{a private object.}
  \item{df}{a data frame which should have column names.}
  \item{name}{name of the heatmap annotation}
  \item{col}{a list which contains color mapping to columns in \code{df}.}
  \item{show_legend}{whether show legend.}
  \item{...}{functions which define complex annotations.}
  \item{which}{are the annotations row annotations or column annotations.}
  \item{annotation_height}{height of each annotation if annotations are column annotations.}
  \item{annotation_width}{width of each annotation if annotations are row annotations.}
  \item{height}{not using currently.}
  \item{width}{width of the whole heatmap annotations, only used for column annotation when appending to the list of heatmaps.}

}
\details{
The simple annotations are defined by \code{df} and \code{col} arguments, complex annotations are defined by the function list.   


}
\value{
A \code{\link{HeatmapAnnotation}} object.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
