\name{component_height-Heatmap-method}
\alias{component_height,Heatmap-method}
\title{
Heights of Heatmap Components
}
\description{
Heights of Heatmap Components
}
\usage{
\S4method{component_height}{Heatmap}(object, k = HEATMAP_LAYOUT_COLUMN_COMPONENT)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{k}{Which components in the heatmap. The value should numeric indices or the names of the corresponding column component. See **Detials**.}

}
\details{
All column components are: \code{column_title_top}, \code{column_dend_top}, \code{column_names_top}, 
\code{column_anno_top}, \code{heatmap_body}, \code{column_anno_bottom}, \code{column_names_bottom}, 
\code{column_dend_bottom}, \code{column_title_bottom}.

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
