\name{Heatmap3D}
\alias{Heatmap3D}
\title{
3D Heatmap
}
\description{
3D Heatmap
}
\usage{
Heatmap3D(matrix,
    ...,
    
    bar_rel_width = 0.6,
    bar_rel_height = 0.6,
    bar_max_length = unit(1, "cm"),
    bar_angle = 60,
    
    row_names_side = "left",
    show_row_dend = FALSE,
    show_column_dend = FALSE)
}
\arguments{

  \item{matrix}{The input matrix. Values should be non-negative.}
  \item{...}{All pass to \code{\link{Heatmap}}.}
  \item{bar_rel_width}{A factor between 0 and 1.}
  \item{bar_rel_height}{A factor between 0 and 1.}
  \item{bar_max_length}{Maximal length of bars. Value should be in absolute unit.}
  \item{bar_angle}{Angle for the projection.}
  \item{row_names_side}{Row names are by default put on the left side of the heatmap.}
  \item{show_row_dend}{By default the dendrogram is not drawn.}
  \item{show_column_dend}{By default the dendrogram is not drawn.}

}
\section{Detals}{
For large matrices, the plotting might be slow.}
\examples{
m = matrix(sample(100, 36), 6)
Heatmap3D(m)
}
