\name{ht_global_opt}
\alias{ht_global_opt}
\title{
Global options

}
\description{
Global options

}
\usage{
ht_global_opt(..., RESET = FALSE, READ.ONLY = NULL)}
\arguments{

  \item{...}{options, see 'details' section}
  \item{RESET}{reset all the options}
  \item{READ.ONLY}{how to return read-only options}
}
\details{
You can set some parameters for all heatmaps/annotations by this global function.
There are following parameters:

\describe{
  \item{heatmap_row_names_gp}{set \code{row_names_gp} in \code{\link{Heatmap}}.}
  \item{heatmap_column_names_gp}{set \code{column_names_gp} in \code{\link{Heatmap}}.}
  \item{heatmap_row_title_gp}{set \code{\link{row_title_gp}}\code{\link{ in }}Heatmap`.}
  \item{heatmap_column_title_gp}{set \code{\link{column_title_gp}} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_title_gp}{set \code{title_gp} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_labels_gp}{set \code{labels_gp} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_grid_width}{set \code{grid_width} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_grid_height}{set \code{grid_height} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_grid_border}{set \code{grid_border} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_title_gp}{set \code{title_gp} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_labels_gp}{set \code{labels_gp} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_grid_width}{set \code{grid_width} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_grid_height}{set \code{grid_height} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_grid_border}{set \code{grid_border} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
}

}
\author{
Zuguang Gu <z.gu@dkfz.de>

}
