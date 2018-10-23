\name{Heatmap-class}
\docType{class}
\alias{Heatmap-class}
\title{
Class for a Single Heatmap
}
\description{
Class for a Single Heatmap
}
\details{
The \code{\link{Heatmap-class}} is not responsible for heatmap legend and annotation legends. The \code{\link{draw,Heatmap-method}} method
constructs a \code{\link{HeatmapList-class}} object which only contains one single heatmap
and call \code{\link{draw,HeatmapList-method}} to make the complete heatmap.
}
\section{Methods}{
The \code{\link{Heatmap-class}} provides following methods:

\itemize{
  \item \code{\link{Heatmap}}: constructor method.
  \item \code{\link{draw,Heatmap-method}}: draw a single heatmap.
  \item \code{\link{add_heatmap,Heatmap-method}} append heatmaps and annotations to a list of heatmaps.
  \item \code{\link{row_order,HeatmapList-method}}: get order of rows
  \item \code{\link{column_order,HeatmapList-method}}: get order of columns
  \item \code{\link{row_dend,HeatmapList-method}}: get row dendrograms
  \item \code{\link{column_dend,HeatmapList-method}}: get column dendrograms
}}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
