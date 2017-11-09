\name{packLegend}
\alias{packLegend}
\title{
Pack legends
}
\description{
Pack legends
}
\usage{
packLegend(..., gap = unit(4, "mm"), direction = c("vertical", "horizontal"))
}
\arguments{

  \item{...}{objects returned by \code{\link{Legend}}}
  \item{gap}{gap between two legends. The value is a \code{\link[grid]{unit}} object}
  \item{direction}{how to arrange legends}

}
\value{
A \code{\link[grid]{grob}} object
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
lgd1 = Legend(title = "discrete", at = 1:4, labels = letters[1:4], 
	legend_gp = gpar(fill = 2:5))

require(circlize)
col_fun = colorRamp2(c(-1, 0, 1), c("blue", "white", "red"))
lgd2 = Legend(title = "continuous", at = seq(-1, 1, by = 0.5), col_fun = col_fun)

pl = packLegend(lgd1, lgd2)
grid.newpage()
grid.draw(pl)

pl = packLegend(lgd1, lgd2, direction = "horizontal")
grid.newpage()
grid.draw(pl)
}
