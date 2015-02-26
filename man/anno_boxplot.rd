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

  \item{x}{a matrix or a list}
  \item{which}{column annotation or row annotation}
  \item{gp}{graphic parameters}
  \item{pch}{point type}
  \item{size}{point size}

}
\value{
A function  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
mat = matrix(rnorm(32), nrow = 4)
f = anno_boxplot(mat)
f(1:8)

f = anno_boxplot(mat, which = "row")
f(1:4)

lt = lapply(1:4, function(i) rnorm(8))
f = anno_boxplot(lt)
f(1:8)	
}
