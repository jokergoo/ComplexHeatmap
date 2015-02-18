\name{show-Heatmap-method}
\alias{show,Heatmap-method}
\title{
Draw the single heatmap with default parameters  


}
\description{
Draw the single heatmap with default parameters  


}
\usage{
\S4method{show}{Heatmap}(object)
}
\arguments{

  \item{object}{a \code{\link{Heatmap}} object.}

}
\details{
Actually it calls \code{\link{draw,Heatmap-method}}, but only with default parameters. If users want to customize the heatmap, they can pass parameters directly to \code{\link{draw,Heatmap-method}}.  


}
\value{
This function returns no value.  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
