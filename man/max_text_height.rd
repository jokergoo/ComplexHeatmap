\name{max_text_height}
\alias{max_text_height}
\title{
Maximum Height of Text
}
\description{
Maximum Height of Text
}
\usage{
max_text_height(text, gp = gpar(), rot = 0)
}
\arguments{

  \item{text}{A vector of text.}
  \item{gp}{Graphic parameters for text.}
  \item{rot}{Rotation of the text, scalar.}

}
\details{
It simply calculates maximum height of a list of \code{\link[grid:grid.text]{textGrob}} objects.

Note it ignores the text rotation.
}
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\seealso{
\code{\link{max_text_width}} calculates the maximum width of a text vector.
}
\examples{
x = c("a", "b\nb", "c\nc\nc")
max_text_height(x, gp = gpar(fontsize = 10))
}
