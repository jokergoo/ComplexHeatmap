\name{max_text_width}
\alias{max_text_width}
\title{
Maximum width of text
}
\description{
Maximum width of text
}
\usage{
max_text_width(text, ...)
}
\arguments{

  \item{text}{a vector of text}
  \item{...}{pass to \code{\link[grid]{textGrob}}}

}
\details{
Simply calculate maximum width of a list of \code{\link[grid]{textGrob}} objects.
}
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\seealso{
\code{\link{max_text_width}} is always used to calculate the size of viewport when there is text annotation (\code{\link{anno_text}})
}
\examples{
x = c("a", "bb", "ccc")
max_text_width(x, gp = gpar(fontsize = 10))
}
