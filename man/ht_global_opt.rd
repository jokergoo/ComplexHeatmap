\name{ht_global_opt}
\alias{ht_global_opt}
\title{
Global graphic options for heatmaps
}
\description{
Global graphic options for heatmaps
}
\usage{
ht_global_opt(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE)
}
\arguments{

  \item{...}{options, see 'details' section}
  \item{RESET}{reset all the option values}
  \item{READ.ONLY}{\code{TRUE} means only to return read-only values, \code{FALSE} means only to return non-read-only values, \code{NULL} means to return both.}
  \item{LOCAL}{switch local mode}

}
\details{
You can set some parameters for all heatmaps/annotations simultaneously by this global function.
Pleast note you should put it before your heatmap code and reset
all option values after drawing the heatmaps to get rid of affecting next heatmap plotting.

There are following parameters:

\describe{
  \item{heatmap_row_names_gp}{set \code{row_names_gp} in \code{\link{Heatmap}}.}
  \item{heatmap_column_names_gp}{set \code{column_names_gp} in \code{\link{Heatmap}}.}
  \item{heatmap_row_title_gp}{set \code{row_title_gp} in \code{\link{Heatmap}}.}
  \item{heatmap_column_title_gp}{set \code{column_title_gp} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_title_gp}{set \code{title_gp} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_title_position}{set \code{title_position} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_labels_gp}{set \code{labels_gp} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_grid_width}{set \code{grid_width} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_grid_height}{set \code{grid_height} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_grid_border}{set \code{grid_border} element in \code{heatmap_legend_param} in \code{\link{Heatmap}}.}
  \item{heatmap_legend_title_gp}{set \code{title_gp} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_title_position}{set \code{title_position} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_labels_gp}{set \code{labels_gp} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_grid_width}{set \code{grid_width} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_grid_height}{set \code{grid_height} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
  \item{heatmap_legend_grid_border}{set \code{grid_border} element in \code{legend_param} in \code{\link{SingleAnnotation}}.}
}

You can get or set option values by the traditional way (like \code{\link[base]{options}}) or by \code{$} operator:

  \preformatted{
    # to get option values
    ht_global_opt("heatmap_row_names_gp")
    ht_global_opt$heatmap_row_names_gp

    # to set option values
    ht_global_opt("heatmap_row_names_gp" = gpar(fontsize = 8))
    ht_global_opt$heatmap_row_names_gp = gpar(fontsize = 8)  }
}
\value{
Depends on the options users selected.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# no example for this function
NULL
}
