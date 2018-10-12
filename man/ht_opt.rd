\name{ht_opt}
\alias{ht_opt}
\title{
Global Options
}
\description{
Global Options
}
\usage{
ht_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE)
}
\arguments{

  \item{...}{options, see 'details' section}
  \item{RESET}{reset all the option values}
  \item{READ.ONLY}{\code{TRUE} means only to return read-only values, \code{FALSE} means only to return non-read-only values, \code{NULL} means to return both.}
  \item{LOCAL}{switch local mode}
  \item{ADD}{add new options}

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

Following parameters to control the legends:

\describe{
  \item{legend_title_gp}{set}
  \item{legend_title_position}{-legend_title_position}
  \item{legend_labels_gp}{-legend_labels_gp}
  \item{legend_grid_width}{-legend_grid_width}
  \item{legend_grid_height}{-legend_grid_height}
  \item{legend_border}{-legend_border}
}

Following parameters to control annotations:

\describe{
  \item{annotation_border}{border of all annotations}
  \item{anno_simple_size}{size for the simple annotation.}
}

Following parameters to control the space between heatmap components:

\describe{
  \item{DENDROGRAM_PADDING}{space bewteen dendrograms and heatmap body}
  \item{DIMNAME_PADDING}{space between row/column names and heatmap body}
  \item{TITLE_PADDING}{space between row/column titles and heatmap body}
  \item{COLUMN_ANNO_PADDING}{space between column annotations and heatmap body}
  \item{ROW_ANNO_PADDING}{space between row annotations and heatmap body}
}

Other parameters:

\describe{
  \item{fast_hclust}{whether use \code{\link[fastcluster]{hclust}} to speed up clustering?}
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
# no example for this function
NULL
}
