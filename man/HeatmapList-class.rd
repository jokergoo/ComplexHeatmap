\name{HeatmapList-class}
\docType{class}
\alias{HeatmapList-class}
\title{
Class for a list of heatmaps
}
\description{
Class for a list of heatmaps
}
\details{
A heatmap list is defined as a list of heatmaps and row annotations.

The components for the heamtap list are placed into a 7 x 7 layout:

  \preformatted{
         +------+(1)
         +------+(2)
         +------+(3)
   +-+-+-+------+-+-+-+
   |1|2|3| 4(4) |5|6|7|
   +-+-+-+------+-+-+-+
         +------+(5)
         +------+(6)
         +------+(7)  }

From top to bottom in column 4, the regions are:

\itemize{
  \item annotation legend on the top, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
  \item heatmap legend on the top, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item title for the heatmap list which is put on the top, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item the list of heatmaps and row annotations
  \item title for the heatmap list which is put on the bottom, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item heatmap legend on the bottom, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item annotation legend on the bottom, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
}

From left to right in row 4, the regions are:

\itemize{
  \item annotation legend on the left, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
  \item heatmap legend on the left, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item title for the heatmap list which is put on the left, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item the list of heatmaps and row annotations
  \item title for the heatmap list which is put on the right, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item heatmap legend on the right, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item annotation legend on the right, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
}

For the list of heatmaps which are placed at (5, 5) in the layout, the heatmaps and row annotations
are placed one after the other.
}
\section{Methods}{
The \code{\link{HeatmapList-class}} provides following methods:

\itemize{
  \item \code{\link{draw,HeatmapList-method}}: draw the list of heatmaps and row annotations.
  \item \code{\link{add_heatmap,HeatmapList-method}}: add heatmaps to the list of heatmaps.
  \item \code{\link{row_order,HeatmapList-method}}: get order of rows
  \item \code{\link{column_order,HeatmapList-method}}: get order of columns
  \item \code{\link{row_dend,HeatmapList-method}}: get row dendrograms
  \item \code{\link{column_dend,HeatmapList-method}}: get column dendrograms
}}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(80, 2), 8, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:12]
colnames(mat) = letters[1:10]

ht = Heatmap(mat)
ht + ht
ht + ht + ht

ht_list = ht + ht
ht + ht_list

ha = HeatmapAnnotation(points = anno_points(1:12, which = "row"), 
    which = "row")
ht + ha
ht_list + ha

}
