\name{ht_opt}
\alias{ht_opt}
\title{
Global Options for Heatmaps
}
\description{
Global Options for Heatmaps
}
\usage{
ht_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{Options, see 'Details' section.}
  \item{RESET}{Reset all the option values.}
  \item{READ.ONLY}{Please ignore this argument.}
  \item{LOCAL}{Please ignore this argument.}
  \item{ADD}{Please ignore this argument.}

}
\details{
You can set some parameters for all heatmaps/annotations simultaneously by this global function.
Pleast note you should put it before your heatmap code and reset
all option values after drawing the heatmaps to get rid of affecting next heatmap.

There are following parameters to control all heatmaps:

\describe{
  \item{heatmap_row_names_gp}{set \code{row_names_gp} in all \code{\link{Heatmap}}.}
  \item{heatmap_column_names_gp}{set \code{column_names_gp} in all \code{\link{Heatmap}}.}
  \item{heatmap_row_title_gp}{set \code{row_title_gp} in all \code{\link{Heatmap}}.}
  \item{heatmap_column_title_gp}{set \code{column_title_gp} in all \code{\link{Heatmap}}.}
  \item{heatmap_border}{set \code{border} in all \code{\link{Heatmap}}.}
}

Following parameters control the legends:

\describe{
  \item{legend_title_gp}{set \code{title_gp} in all heatmap legends and annotation legends.}
  \item{legend_title_position}{set \code{title_position} in all heatmap legends and annotation legends.}
  \item{legend_labels_gp}{set \code{labels_gp} in all heatmap legends and annotation legends.}
  \item{legend_grid_width}{set \code{grid_width} in all heatmap legends and annotation legends.}
  \item{legend_grid_height}{set \code{grid_height} in all heatmap legends and annotation legends.}
  \item{legend_border}{set \code{border} in all heatmap legends and annotation legends.}
  \item{legend_gap}{Gap between legends. The value should be a vector of two units. One for gaps between vertical legends and one for the horizontal legends. If only one single unit is specified, the same gap set for the vertical and horizontal legends.}
  \item{merge_legend}{wether merge heatmap and annotation legends.}
}

Following parameters control heatmap annotations:

\describe{
  \item{annotation_border}{\code{border} in all \code{\link{HeatmapAnnotation}}.}
  \item{simple_anno_size}{size for the simple annotation.}
}

Following parameters control the space between heatmap components:

\describe{
  \item{DENDROGRAM_PADDING}{space bewteen dendrograms and heatmap body.}
  \item{DIMNAME_PADDING}{space between row/column names and heatmap body.}
  \item{TITLE_PADDING}{space between row/column titles and heatmap body. The value can have length of two which corresponds to the botton and top padding.}
  \item{COLUMN_ANNO_PADDING}{space between column annotations and heatmap body.}
  \item{ROW_ANNO_PADDING}{space between row annotations and heatmap body.}
  \item{HEATMAP_LEGEND_PADDING}{space between heatmap legends and heatmaps}
  \item{ANNOTATION_LEGEND_PADDING}{space between annotation legends and heatmaps}
}

Other parameters:

\describe{
  \item{fast_hclust}{whether use \code{\link[fastcluster]{hclust}} to speed up clustering?}
  \item{show_parent_dend_line}{when heatmap is split, whether to add a dashed line to mark parent dendrogram and children dendrograms?}
  \item{COLOR}{default colors for continuous color mapping.}
}

You can get or set option values by the traditional way (like \code{\link[base]{options}}) or by \code{$} operator:

  \preformatted{
    # to get option values
    ht_opt("heatmap_row_names_gp")
    ht_opt$heatmap_row_names_gp

    # to set option values
    ht_opt("heatmap_row_names_gp" = gpar(fontsize = 8))
    ht_opt$heatmap_row_names_gp = gpar(fontsize = 8)  }

Reset to the default values by \code{ht_opt(RESET = TRUE)}.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
ht_opt
}
