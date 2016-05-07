\name{rowAnnotation}
\alias{rowAnnotation}
\title{
Construct row annotations
}
\description{
Construct row annotations
}
\usage{
rowAnnotation(..., width = unit(1, "cm"))
}
\arguments{

  \item{...}{pass to \code{\link{HeatmapAnnotation}}}
  \item{width}{default width of the row annotations}

}
\details{
The function is identical to

  \preformatted{
    HeatmapAnnotation(..., which = "row", width = width)  }
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
