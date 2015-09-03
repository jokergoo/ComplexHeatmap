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
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
max_text_height(letters, gp = gpar(fontsize = 10))
}
