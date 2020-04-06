\name{alter_graphic}
\alias{alter_graphic}
\title{
Automatically generate alter_fun
}
\description{
Automatically generate alter_fun
}
\usage{
alter_graphic(graphic = c("rect", "point"),
    width = 1, height = 1,
    horiz_margin = unit(1, "pt"), vertical_margin = unit(1, "pt"),
    fill = "red", col = NA, pch = 16, ...)
}
\arguments{

  \item{graphic}{Graphic to draw.}
  \item{width}{Relative width of the rectangle.}
  \item{height}{Relative height of the rectangle.}
  \item{horiz_margin}{Horizontal margin. E.g. if you want 1mm margin on top and 1mm margin at bottom of the rectangle, set this value to \code{unit(1, 'mm')}.}
  \item{vertical_margin}{Vertical margin.}
  \item{fill}{Filled color.}
  \item{col}{Border color.}
  \item{pch}{Pch for points}
  \item{...}{Pass to \code{\link[grid]{gpar}}}

}
\details{
This function aims to simplify the definition of functions in \code{alter_fun}. Now it only
supports rectangles and points.
}
\examples{
mat = read.table(textConnection(
"s1,s2,s3
g1,snv;indel,snv,indel
g2,,snv;indel,snv
g3,snv,,indel;snv"), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
mat = as.matrix(mat)
col = c(snv = "red", indel = "blue")

oncoPrint(mat, 
	alter_fun = list(
		snv = alter_graphic("rect", width = 0.9, height = 0.9, fill = col["snv"]),
		indel = alter_graphic("rect", width = 0.9, height = 0.9, fill = col["indel"])
	), col = col)
}
