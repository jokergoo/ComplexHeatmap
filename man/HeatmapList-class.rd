\name{HeatmapList-class}
\docType{class}
\alias{HeatmapList-class}
\alias{HeatmapList}
\title{
Class for a list of heatmaps  


}
\description{
Class for a list of heatmaps  


}
\details{
The components for the heamtap list are placed into a 7 x 7 layout:  

  \preformatted{
         +------+
         +------+
         +------+
   +-+-+-+------+-+-+-+
   | | | |      | | | |
   +-+-+-+------+-+-+-+
         +------+
         +------+
         +------+
  }

From top to bottom in column 4, the regions are:  

\itemize{
  \item annotation legend on the top, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
  \item heatmap legend on the top, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item title for the heatmap list which are put on the top, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item the heatmap list
  \item title for the heatmap list which are put on the bottom, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item heatmap legend on the bottom, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item annotation legend on the bottom, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
}

From left to right in row 4, the regions are:  

\itemize{
  \item annotation legend on the left, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
  \item heatmap legend on the left, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item title for the heatmap list which are put on the left, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item the heatmap list
  \item title for the heatmap list which are put on the right, graphics are drawn by \code{\link{draw_title,HeatmapList-method}}.
  \item heatmap legend on the right, graphics are drawn by \code{\link{draw_heatmap_legend,HeatmapList-method}}.
  \item annotation legend on the right, graphics are drawn by \code{\link{draw_annotation_legend,HeatmapList-method}}.
}

For the list of heatmaps which is placed at [5, 5] in the layout, the heatmaps are placed one after the other.  


}
\section{Methods}{
The \code{\link{HeatmapList}} class provides following methods:  

\itemize{
  \item \code{\link{draw,HeatmapList-method}}: draw a single heatmap.
  \item \code{\link{add_heatmap,HeatmapList-method}} add heatmaps to the list of heatmaps.
}


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
