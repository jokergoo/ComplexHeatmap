\name{select}
\alias{select}
\title{
Select a region in the heatmap
}
\description{
Select a region in the heatmap
}
\usage{
select(mark = FALSE)
}
\arguments{

  \item{mark}{whether mark the selected region as a rectangle}

}
\details{
Users can use mouse to click two positions on the heatmap, the function
will return the row index and column index for the selected region in the selected matrix.

Of cource this function only works under interactive graphical environment.
}
\value{
A list containing row index and column index corresponding to the selected region.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# No example for this function
NULL
}
