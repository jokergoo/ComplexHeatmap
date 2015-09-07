\name{rowAnnotation}
\alias{rowAnnotation}
\title{
Construct row annotations
}
\description{
Construct row annotations
}
\usage{
rowAnnotation(...)
}
\arguments{

  \item{...}{pass to \code{\link{HeatmapAnnotation}}}

}
\details{
The function is identical to

  \preformatted{
    HeatmapAnnotation(..., which = "row")  }
}
\value{
A \code{\link{HeatmapAnnotation-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
df = data.frame(type = c("a", "a", "a", "b", "b", "b"))
ha = columnAnnotation(df = df)

}
