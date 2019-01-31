\name{test_alter_fun}
\alias{test_alter_fun}
\title{
Test alter_fun for oncoPrint()
}
\description{
Test alter_fun for oncoPrint()
}
\usage{
test_alter_fun(fun, type, asp_ratio = 1)
}
\arguments{

  \item{fun}{The \code{alter_fun} for \code{\link{oncoPrint}}. The value can be a list of functions or a single function. See \url{https://jokergoo.github.io/ComplexHeatmap-reference/book/oncoprint.html#define-the-alter-fun}}
  \item{type}{A vector of alteration types. It is only used when \code{fun} is a single function.}
  \item{asp_ratio}{The aspect ratio (width/height) for the small rectangles.}

}
\details{
This function helps you to have a quick view of how the graphics for each alteration type
and combinations look like.
}
\examples{
alter_fun = list(
	mut1 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "red", col = NA)),
	mut2 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "blue", col = NA)),
	mut3 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "yellow", col = NA)),
	mut4 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(fill = "purple", col = NA)),
	mut5 = function(x, y, w, h) grid.rect(x, y, w, h, gp = gpar(lwd = 2)),
	mut6 = function(x, y, w, h) grid.points(x, y, pch = 16),
	mut7 = function(x, y, w, h) grid.segments(x - w*0.5, y - h*0.5, x + w*0.5, y + h*0.5, gp = gpar(lwd = 2))
)
test_alter_fun(alter_fun)
}
