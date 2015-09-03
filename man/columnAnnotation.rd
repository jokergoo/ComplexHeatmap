\name{columnAnnotation}
\alias{columnAnnotation}
\title{
Construct column annotations
}
\description{
Construct column annotations
}
\usage{
columnAnnotation(...)
}
\arguments{

  \item{...}{pass to \code{\link{HeatmapAnnotation}}}

}
\details{
The function is identical to

  \preformatted{
    HeatmapAnnotation(..., which = "column")  }
}
\value{
A \code{\link{HeatmapAnnotation-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
df = data.frame(type = c("a", "a", "a", "b", "b", "b"))
ha = rowAnnotation(df = df)

}
