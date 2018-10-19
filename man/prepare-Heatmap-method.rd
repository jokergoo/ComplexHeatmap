\name{prepare-Heatmap-method}
\alias{prepare,Heatmap-method}
\alias{prepare}
\title{
Prepare the Heatmap
}
\description{
Prepare the Heatmap
}
\usage{
\S4method{prepare}{Heatmap}(object, process_rows = TRUE, process_columns = TRUE)
}
\arguments{

  \item{object}{A \code{\link{Heatmap-class}} object.}
  \item{process_rows}{Whether to process rows of the heatmap.}
  \item{process_columns}{Whether to process columns of the heatmap.}

}
\details{
The preparation of the heatmap includes following steps:

\itemize{
  \item making clustering on rows (by calling \code{\link{make_row_cluster,Heatmap-method}})
  \item making clustering on columns (by calling \code{\link{make_column_cluster,Heatmap-method}})
  \item making the layout of the heatmap (by calling \code{\link{make_layout,Heatmap-method}})
}

This function is only for internal use.
}
\value{
The \code{\link{Heatmap-class}} object.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
# There is no example
NULL

}
