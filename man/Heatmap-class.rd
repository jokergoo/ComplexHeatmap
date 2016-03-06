\name{Heatmap-class}
\docType{class}
\alias{Heatmap-class}
\title{
Class for a single heatmap
}
\description{
Class for a single heatmap
}
\details{
The components for a single heamtap are placed into a 9 x 7 layout:

  \preformatted{
         +------+ (1)
         +------+ (2)
         +------+ (3)
         +------+ (4)
   +-+-+-+------+-+-+-+
   |1|2|3| 4(5) |5|6|7|
   +-+-+-+------+-+-+-+
         +------+ (6)
         +------+ (7)
         +------+ (8)
         +------+ (9)  }

From top to bottom in column 4, the regions are:

\itemize{
  \item title which is put on the top of the heatmap, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
  \item column cluster on the top, graphics are drawn by \code{\link{draw_dend,Heatmap-method}}.
  \item column annotation on the top, graphics are drawn by \code{\link{draw_annotation,Heatmap-method}}.
  \item column names on the top, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item heatmap body, graphics are drawn by \code{\link{draw_heatmap_body,Heatmap-method}}.
  \item column names on the bottom, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item column annotation on the bottom, graphics are drawn by \code{\link{draw_annotation,Heatmap-method}}.
  \item column cluster on the bottom, graphics are drawn by \code{\link{draw_dend,Heatmap-method}}.
  \item title on the bottom, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
}

From left to right in row 5, the regions are:

\itemize{
  \item title which is put in the left of the heatmap, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
  \item row cluster on the left, graphics are drawn by \code{\link{draw_dend,Heatmap-method}}.
  \item row names on the left, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item heatmap body
  \item row names on the right, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item row cluster on the right, graphics are drawn by \code{\link{draw_dend,Heatmap-method}}.
  \item title on the right, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
}

The \code{\link{Heatmap-class}} is not responsible for heatmap legend and annotation legends. The \code{\link{draw,Heatmap-method}} method
will construct a \code{\link{HeatmapList-class}} object which only contains one single heatmap
and call \code{\link{draw,HeatmapList-method}} to make a complete heatmap.
}
\section{Methods}{
The \code{\link{Heatmap-class}} provides following methods:

\itemize{
  \item \code{\link{Heatmap}}: constructor method.
  \item \code{\link{draw,Heatmap-method}}: draw a single heatmap.
  \item \code{\link{add_heatmap,Heatmap-method}} append heatmaps and row annotations to a list of heatmaps.
  \item \code{\link{row_order,HeatmapList-method}}: get order of rows
  \item \code{\link{column_order,HeatmapList-method}}: get order of columns
  \item \code{\link{row_dend,HeatmapList-method}}: get row dendrograms
  \item \code{\link{column_dend,HeatmapList-method}}: get column dendrograms
}}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# for examples, please go to `Heatmap` method page
NULL

}
