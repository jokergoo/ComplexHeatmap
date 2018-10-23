\name{anno_oncoprint_barplot}
\alias{anno_oncoprint_barplot}
\title{
Barplot Annotation for oncoPrint
}
\description{
Barplot Annotation for oncoPrint
}
\usage{
anno_oncoprint_barplot(type = all_type, which = c("column", "row"),
    width = NULL, height = NULL, border = FALSE, ...)
}
\arguments{

  \item{type}{A vector of the alteration types in the data. It can be a subset of all alteration types if you don't want to show them all.}
  \item{which}{Is it a row annotation or a column annotation?}
  \item{width}{Wisth of the annotation.}
  \item{height}{Height of the annotation.}
  \item{border}{Whether draw the border?}
  \item{...}{Other parameters passed to \code{\link{anno_barplot}}.}

}
\details{
This annotation function should always be used with \code{\link{oncoPrint}}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
