\name{component_width-Heatmap-method}
\alias{component_width,Heatmap-method}
\title{
Widths of Heatmap Components
}
\description{
Widths of Heatmap Components
}
\usage{
\S4method{component_width}{Heatmap}(object, k = HEATMAP_LAYOUT_ROW_COMPONENT)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{k}{Which components in the heatmap. The value should numeric indices or the names of the corresponding row component. See **Detials**.}

}
\details{
All row components are: \code{row_title_left}, \code{row_dend_left}, \code{row_names_left}, \code{row_anno_left},
\code{heatmap_body}, \code{row_anno_right}, \code{row_names_right}, \code{row_dend_right}, \code{row_title_right}.

This function is only for internal use.
}
\value{
A \code{\link[grid]{unit}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
