\name{pheatmap}
\alias{pheatmap}
\title{
Translate pheatmap::pheatmap to ComplexHeatmap::Heatmap
}
\description{
Translate pheatmap::pheatmap to ComplexHeatmap::Heatmap
}
\usage{
pheatmap(mat,
    color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100),
    kmeans_k = NA,
    breaks = NA,
    border_color = ifelse(nrow(mat) < 100 & ncol(mat) < 100, "grey60", NA),
    cellwidth = NA,
    cellheight = NA,
    scale = "none",
    cluster_rows = TRUE,
    cluster_cols = TRUE,
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean",
    clustering_method = "complete",
    clustering_callback = NA,
    cutree_rows = NA,
    cutree_cols = NA,
    treeheight_row = ifelse(class(cluster_rows) == "hclust" || cluster_rows, 50, 0),
    treeheight_col = ifelse(class(cluster_cols) == "hclust" || cluster_cols, 50, 0),
    legend = TRUE,
    legend_breaks = NA,
    legend_labels = NA,
    annotation_row = NA,
    annotation_col = NA,
    annotation = NA,
    annotation_colors = NA,
    annotation_legend = TRUE,
    annotation_names_row = TRUE,
    annotation_names_col = TRUE,
    drop_levels = TRUE,
    show_rownames = TRUE,
    show_colnames = TRUE,
    main = NA,
    fontsize = 10,
    fontsize_row = fontsize,
    fontsize_col = fontsize,
    angle_col = c("270", "0", "45", "90", "315"),
    display_numbers = FALSE,
    number_format = "\%.2f",
    number_color = "grey30",
    fontsize_number = 0.8 * fontsize,
    gaps_row = NULL,
    gaps_col = NULL,
    labels_row = NULL,
    labels_col = NULL,
    filename = NA,
    width = NA,
    height = NA,
    silent = FALSE,
    na_col = "#DDDDDD",
    name = NULL,
    
    # other graphic parameters for fonts
    fontfamily = "",
    fontfamily_row = fontfamily,
    fontfamily_col = fontfamily,
    fontface = 1,
    fontface_row = fontface,
    fontface_col = fontface,
    
    # argument specific for Heatmap()
    heatmap_legend_param = list(),
    ...,
    run_draw = FALSE)
}
\arguments{

  \item{mat}{The input matrix.}
  \item{color}{The same as in \code{\link[pheatmap]{pheatmap}}. Here you don't necessarily need to generate a long color vector. The discrete colors sent to \code{\link[grDevices:colorRamp]{colorRampPalette}} are also OK here. E.g. \code{colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)} can be simply  replaced as \code{rev(brewer.pal(n = 7, name = "RdYlBu"))}.}
  \item{kmeans_k}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{breaks}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{border_color}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{cellwidth}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{cellheight}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{scale}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{cluster_rows}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{cluster_cols}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{clustering_distance_rows}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{clustering_distance_cols}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{clustering_method}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{clustering_callback}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{cutree_rows}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{cutree_cols}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{treeheight_row}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{treeheight_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{legend}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{legend_breaks}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{legend_labels}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation_row}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation_colors}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation_legend}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation_names_row}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{annotation_names_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{drop_levels}{Enforced to be \code{TRUE}.}
  \item{show_rownames}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{show_colnames}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{main}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{fontsize}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{fontsize_row}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{fontsize_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{angle_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{display_numbers}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{number_format}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{number_color}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{fontsize_number}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{gaps_row}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{gaps_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{labels_row}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{labels_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{filename}{Not supported.}
  \item{width}{Not supported.}
  \item{height}{Not supported.}
  \item{silent}{Not supported.}
  \item{na_col}{The same as in \code{\link[pheatmap]{pheatmap}}.}
  \item{name}{Name of the heatmap. This argument is passed to \code{\link{Heatmap}}.}
  \item{fontfamily}{Font family for row and column names.}
  \item{fontfamily_row}{Font family for row names.}
  \item{fontfamily_col}{Font family for column names.}
  \item{fontface}{Font face for row and column names.}
  \item{fontface_row}{Font face for row names.}
  \item{fontface_col}{Font face for column names.}
  \item{heatmap_legend_param}{Pass to \code{\link{Heatmap}}.}
  \item{...}{Other arguments passed to \code{\link{Heatmap}}.}
  \item{run_draw}{Whether to run \code{draw()} function to the heatmap object.}

}
\details{
This function aims to execute \code{pheatmap::pheatmap} code purely with ComplexHeatmap.
}
\seealso{
See \url{https://jokergoo.github.io/2020/05/06/translate-from-pheatmap-to-complexheatmap/}

\code{\link{compare_pheatmap}} that compares heatmaps between \code{pheatmap::pheatmap()} and \code{ComplexHeatmap::pheatmap()}.
}
\value{
A \code{\link{Heatmap-class}} object.
}
\examples{
# There is no example
NULL

}
