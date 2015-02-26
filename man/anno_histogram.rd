\name{anno_histogram}
\alias{anno_histogram}
\title{
Using histogram as annotation  


}
\description{
Using histogram as annotation  


}
\usage{
anno_histogram(x, which = c("column", "row"), gp = gpar())
}
\arguments{

  \item{x}{a vector}
  \item{which}{column annotation or row annotation}
  \item{gp}{graphic parameters}

}
\value{
A function  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
f = anno_histogram(rnorm(10))
f(1:10)

f = anno_histogram(rnorm(10), which = "row")
f(1:10)
}
