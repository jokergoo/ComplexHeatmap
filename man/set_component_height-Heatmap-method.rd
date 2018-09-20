\name{set_component_height-Heatmap-method}
\alias{set_component_height,Heatmap-method}
\alias{set_component_height}
\title{
Set Height of Heatmap Component
}
\description{
Set Height of Heatmap Component
}
\usage{
\S4method{set_component_height}{Heatmap}(object, k, v)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{k}{Which column component? The value should a numeric index or the name of the corresponding column component. See **Detials**.}
  \item{v}{Height of the component, a \code{\link[grid]{unit}} object.}

}
\details{
All column components are: \code{column_title_top}, \code{column_dend_top}, \code{column_names_top}, 
\code{column_anno_top}, \code{heatmap_body}, \code{column_anno_bottom}, \code{column_names_bottom}, 
\code{column_dend_bottom}, \code{column_title_bottom}.

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
