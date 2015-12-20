\name{max_text_height}
\alias{max_text_height}
\title{
Maximum height of text
}
\description{
Maximum height of text
}
\usage{
max_text_height(text, ...)
}
\arguments{

  \item{text}{a vector of text}
  \item{...}{pass to \code{\link[grid]{textGrob}}}

}
\details{
Simply calculate maximum height of a list of \code{\link[grid]{textGrob}} objects.
}
\value{
A \code{\link[grid]{unit}} object.
}
\seealso{
\code{\link{max_text_height}} is always used to calculate the size of viewport when there is text annotation (\code{\link{anno_text}})
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
x = c("a", "b\nb", "c\nc\nc")
max_text_height(x, gp = gpar(fontsize = 10))
}
