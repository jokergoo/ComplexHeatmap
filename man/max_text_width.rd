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
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
max_text_width(letters, gp = gpar(fontsize = 10))
}
