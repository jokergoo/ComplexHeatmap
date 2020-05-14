\name{selectArea_shiny_demo}
\alias{selectArea_shiny_demo}
\title{
A demo of using selectArea() as a shiny app
}
\description{
A demo of using selectArea() as a shiny app
}
\usage{
selectArea_shiny_demo(ht_list)
}
\arguments{

  \item{ht_list}{A \code{\link{Heatmap-class}} or a \code{\link{HeatmapList-class}} object.}

}
\details{
source code of the app is at \code{system.file("app", package = "ComplexHeatmap")}.
}
\examples{
if(interactive()) {
    selectArea_shiny_demo()
}

# by providing a heatmap/heatmap list
if(interactive()) {
    m = matrix(rnorm(100), 10)
    rownames(m) = 1:10
    colnames(m) = 1:10

ht = Heatmap(m)
    selectArea_shiny_demo(ht)
}
}
