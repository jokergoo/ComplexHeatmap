\name{make_layout-HeatmapList-method}
\alias{make_layout,HeatmapList-method}
\title{
make layout  


}
\description{
make layout  


}
\usage{
\S4method{make_layout}{HeatmapList}(object,
    row_title = character(0), row_title_side = c("left", "right"), row_title_gp = gpar(fontsize = 14),
    column_title = character(0), column_title_side = c("top", "bottom"), column_title_gp = gpar(fontsize = 14),
    heatmap_legend_side = c("right", "left", "bottom", "top"), show_heatmap_legend = TRUE,
    annotation_legend_side = c("right", "left", "bottom", "top"), show_annotation_legend = TRUE,
    hgap = unit(5, "mm"), vgap = unit(3, "mm"), auto_adjust = TRUE)
}
\arguments{

  \item{object}{a \code{\link{HeatmapList}} object}
  \item{row_title}{title on the row}
  \item{row_title_side}{side of the row title}
  \item{row_title_gp}{graphic parameters for drawing text}
  \item{column_title}{title on the column}
  \item{column_title_side}{side of the column title}
  \item{column_title_gp}{graphic parameters for drawing text}
  \item{heatmap_legend_side}{side of the heatmap legend}
  \item{show_heatmap_legend}{whether show heatmap legend}
  \item{annotation_legend_side}{side of annotation legend}
  \item{show_annotation_legend}{whether show annotation legend}
  \item{hgap}{gap between heatmaps}
  \item{vgap}{gap between heatmaps}
  \item{auto_adjust}{auto adjust if the number of heatmap is larger than one.}

}
\details{
it makes layout  


}
\value{
a \code{\link{HeatmapList}} object  


}
\alias{make_layout}
