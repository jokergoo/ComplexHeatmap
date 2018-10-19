\name{set_component_width-Heatmap-method}
\alias{set_component_width,Heatmap-method}
\alias{set_component_width}
\title{
Set Width of Heatmap Component
}
\description{
Set Width of Heatmap Component
}
\usage{
\S4method{set_component_width}{Heatmap}(object, k, v)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{k}{Which row component? The value should a numeric index or the name of the corresponding row component. See **Detials**.}
  \item{v}{width of the component, a \code{\link[grid]{unit}} object.}

}
\details{
All row components are: \code{row_title_left}, \code{row_dend_left}, \code{row_names_left}, \code{row_anno_left},
\code{heatmap_body}, \code{row_anno_right}, \code{row_names_right}, \code{row_dend_right}, \code{row_title_right}.

This function is only for internal use.
}
\value{
The \code{\link{Heatmap-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
