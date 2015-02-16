\name{Heatmap-class}
\docType{class}
\alias{Heatmap-class}
\alias{Heatmap}
\title{
Class for a single heatmap  


}
\description{
Class for a single heatmap  


}
\details{
The components for a single heamtap are placed into a 9 x 7 layout:  

  \preformatted{
         +------+
         +------+
         +------+
         +------+
   +-+-+-+------+-+-+-+
   | | | |      | | | |
   +-+-+-+------+-+-+-+
         +------+
         +------+
         +------+
         +------+
  }

From top to bottom in column 4, the regions are:  

\itemize{
  \item title put on column, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
  \item column cluster, graphics are drawn by \code{\link{draw_hclust,Heatmap-method}}.
  \item column annotation, graphics are drawn by \code{\link{draw_annotation,Heatmap-method}}.
  \item column names, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item heatmap body, graphics are drawn by \code{\link{draw_heatmap_body,Heatmap-method}}.
  \item column names, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item column annotation, graphics are drawn by \code{\link{draw_annotation,Heatmap-method}}.
  \item column cluster, graphics are drawn by \code{\link{draw_hclust,Heatmap-method}}.
  \item title put on column, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
}

From left to right in row 5, the regions are:  

\itemize{
  \item title put on row, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
  \item row cluster, graphics are drawn by \code{\link{draw_hclust,Heatmap-method}}.
  \item row names, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item heatmap body
  \item row names, graphics are drawn by \code{\link{draw_dimnames,Heatmap-method}}.
  \item row cluster, graphics are drawn by \code{\link{draw_hclust,Heatmap-method}}.
  \item title put on row, graphics are drawn by \code{\link{draw_title,Heatmap-method}}.
}

The \code{\link{Heatmap}} class is not resposible for heatmap legend. The \code{\link{draw,Heatmap-method}} method will construct a \code{\link{HeatmapList}} class which only contains one single heatmap and call \code{\link{draw,HeatmapList-method}} to make a complete heatmap.  


}
\section{Methods}{
The \code{\link{Heatmap}} class provides following methods:  

\itemize{
  \item \code{\link{initialize,Heatmap-method}}: contructor method.
  \item \code{\link{draw,Heatmap-method}}: draw a single heatmap.
  \item \code{\link{add_heatmap,Heatmap-method}} add heatmaps to a list of heatmaps.
}


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
