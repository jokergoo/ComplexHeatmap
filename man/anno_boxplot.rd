\name{anno_boxplot}
\alias{anno_boxplot}
\title{
Using boxplot as annotation  


}
\description{
Using boxplot as annotation  


}
\usage{
anno_boxplot(x, which = c("column", "row"), gp = gpar(fill = "#CCCCCC"),
    pch = 16, size = unit(2, "mm"))
}
\arguments{

  \item{x}{a matrix or a list. If \code{x} is a matrix and if \code{which} is \code{column}, statistics for boxplot is calculated by columns, if \code{which} is \code{row}, the calculation is by rows.}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters}
  \item{pch}{point type}
  \item{size}{point size}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
mat = matrix(rnorm(32), nrow = 4)
f = anno_boxplot(mat)
grid.newpage(); f(1:8)

f = anno_boxplot(mat, which = "row")
grid.newpage(); f(1:4)

lt = lapply(1:4, function(i) rnorm(8))
f = anno_boxplot(lt)
grid.newpage(); f(1:4)
}
