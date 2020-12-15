\name{heatmap}
\alias{stats_heatmap}
\alias{heatmap}
\title{
Translate stats::heatmap to ComplexHeatmap::Heatmap
}
\description{
Translate stats::heatmap to ComplexHeatmap::Heatmap
}
\usage{
heatmap(x,
    col = hcl.colors(12, "YlOrRd", rev = TRUE),
    Rowv = NULL,
    Colv = NULL,
    distfun = dist,
    hclustfun = hclust,
    reorderfun = function(d, w) reorder(d, w),
    add.expr,
    symm = FALSE,
    revC = identical(Colv, "Rowv"),
    scale = c("row", "column", "none"),
    na.rm = TRUE,
    margins = c(5, 5),
    ColSideColors,
    RowSideColors,
    cexRow = 0.6,
    cexCol = 0.6,
    labRow = NULL,
    labCol = NULL,
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    keep.dendro = FALSE,
    verbose = getOption("verbose"),
    ...)
}
\arguments{

  \item{x}{The input matrix.}
  \item{col}{A vector of colors.}
  \item{Rowv}{The same as in \code{\link[stats]{heatmap}}.}
  \item{Colv}{The same as in \code{\link[stats]{heatmap}}.}
  \item{distfun}{The same as in \code{\link[stats]{heatmap}}.}
  \item{hclustfun}{The same as in \code{\link[stats]{heatmap}}.}
  \item{reorderfun}{The same as in \code{\link[stats]{heatmap}}.}
  \item{add.expr}{Ignored.}
  \item{symm}{Ignored.}
  \item{revC}{Ignored.}
  \item{scale}{The same as in \code{\link[stats]{heatmap}}.}
  \item{na.rm}{Ignored.}
  \item{margins}{Ignored.}
  \item{ColSideColors}{The same as in \code{\link[stats]{heatmap}}.}
  \item{RowSideColors}{The same as in \code{\link[stats]{heatmap}}.}
  \item{cexRow}{The same as in \code{\link[stats]{heatmap}}.}
  \item{cexCol}{The same as in \code{\link[stats]{heatmap}}.}
  \item{labRow}{The same as in \code{\link[stats]{heatmap}}.}
  \item{labCol}{The same as in \code{\link[stats]{heatmap}}.}
  \item{main}{The same as in \code{\link[stats]{heatmap}}.}
  \item{xlab}{The same as in \code{\link[stats]{heatmap}}.}
  \item{ylab}{The same as in \code{\link[stats]{heatmap}}.}
  \item{keep.dendro}{Ignored.}
  \item{verbose}{Ignored.}
  \item{...}{Other arguments passed to \code{\link{Heatmap}}.}

}
\details{
This function aims to execute \code{stats::heatmap} code purely with ComplexHeatmap.
}
\value{
A \code{\link{Heatmap-class}} object.
}
\seealso{
\code{\link{compare_heatmap}} that compares heatmaps between \code{stats::heatmap()} and \code{ComplexHeatmap::heatmap()}.
}
\examples{
# There is no example
NULL

}
