\name{plotDataFrame}
\alias{plotDataFrame}
\title{
Visualize a data frame  


}
\description{
Visualize a data frame  


}
\usage{
plotDataFrame(df, overlap = 0.5, nlevel = 30, show_row_names = TRUE,
    group = NULL, group_name = names(group), cluster_rows = TRUE,
    cluster_columns = TRUE)
}
\arguments{

  \item{df}{a data frame}
  \item{overlap}{how to group numeric columns. If the overlapping rate between the ranges in the current column and previous numeric column is larger than this value, the two columns are thought under same measurement and should be grouped.}
  \item{nlevel}{If the number of levels of a character column is larger than this value, the column will be excluded.}
  \item{show_row_names}{whether show row names after the last heatmap if there are row names.}
  \item{group}{a list of index that defines the groupping}
  \item{group_name}{names for each group}
  \item{cluster_rows}{whether perform clustering on rows of the first heatmap}
  \item{cluster_columns}{whether perform clustering on columns for all heatmaps}

}
\details{
The data frame contains information from different aspects and different measurements are applied on the same element for each row. The \code{\link{plotDataFrame}} function provides a simple and quick way to visualize information that are stored in a data frame.  

There are not many advanced settings in this function. Users can customize the style of the heatmaps by constructing a \code{\link{HeatmapList}} object.  


}
\value{
a \code{\link{HeatmapList}} object  


}
\author{
Zuguang Gu <z.gu@dkfz.de>  


}
\examples{
df = data.frame(x = rnorm(10), y = rnorm(10), z = rnorm(10, 10), 
    l1 = letters[1:10], l2 = LETTERS[1:10])
plotDataFrame(df)
plotDataFrame(df, group = list(number = 1:3, letters = 4:5))
}
