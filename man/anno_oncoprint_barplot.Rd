\name{anno_oncoprint_barplot}
\alias{anno_oncoprint_barplot}
\title{
Barplot Annotation for oncoPrint
}
\description{
Barplot Annotation for oncoPrint
}
\usage{
anno_oncoprint_barplot(type = NULL, which = c("column", "row"),
    bar_width = 0.6, beside = FALSE, ylim = NULL, show_fraction = FALSE, axis = TRUE,
    axis_param = if(which == "column") default_axis_param("column") else list(side = "top", labels_rot = 0),
    width = NULL, height = NULL, border = FALSE)
}
\arguments{

  \item{type}{A vector of the alteration types in the data. It can be a subset of all alteration types if you don't want to show them all.}
  \item{which}{Is it a row annotation or a column annotation?}
  \item{bar_width}{Width of the bars.}
  \item{beside}{Will bars be stacked or be positioned beside each other?}
  \item{ylim}{Data range.}
  \item{show_fraction}{Whether to show the numbers or the fractions?}
  \item{axis}{Whether draw axis?}
  \item{axis_param}{Parameters for controlling axis.}
  \item{width}{Width of the annotation.}
  \item{height}{Height of the annotation.}
  \item{border}{Whether draw the border?}

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
