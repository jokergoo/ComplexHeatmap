\name{c.ColorMapping}
\alias{c.ColorMapping}
\title{
Concatenate A List of ColorMapping objects
}
\description{
Concatenate A List of ColorMapping objects
}
\usage{
\method{c}{ColorMapping}(..., name = NULL)
}
\arguments{

  \item{...}{A list of \code{\link{ColorMapping-class}} objects.}
  \item{name}{Name of the new merged color mapping.}

}
\details{
Only discrete color mappings can be concatenated.
}
\examples{
cm1 = ColorMapping(colors = c("A" = "red", "B" = "black"))
cm2 = ColorMapping(colors = c("B" = "blue", "C" = "green"))
c(cm1, cm2)
}
