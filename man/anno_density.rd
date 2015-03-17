\name{anno_density}
\alias{anno_density}
\title{
Using kernel density as annotation  


}
\description{
Using kernel density as annotation  


}
\usage{
anno_density(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"),
    type = c("lines", "violin", "heatmap"), ...)
}
\arguments{

  \item{x}{a matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for density is calculated by columns, if \code{which} is \code{row}, the calculation is by rows.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters}
  \item{type}{which type of graphics is used to represent density distribution}
  \item{...}{passing to \code{\link[stats]{density}}}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
