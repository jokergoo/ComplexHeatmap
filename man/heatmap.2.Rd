\name{heatmap.2}
\alias{heatmap.2}
\title{
Translate gplots::heatmap.2 to ComplexHeatmap::Heatmap
}
\description{
Translate gplots::heatmap.2 to ComplexHeatmap::Heatmap
}
\usage{
heatmap.2(x,
    
    # dendrogram control
    Rowv = TRUE,
    Colv = TRUE,
    distfun = dist,
    hclustfun = hclust,
    dendrogram = c("both","row","column","none"),
    reorderfun = function(d, w) reorder(d, w),
    symm = FALSE,
    
    # data scaling
    scale = c("none","row", "column"),
    na.rm = TRUE,
    
    # image plot
    revC = identical(Colv, "Rowv"),
    add.expr,
    
    # mapping data to colors
    breaks,
    symbreaks = any(x < 0, na.rm = TRUE) || scale != "none",
    
    # colors
    col = "heat.colors",
    
    # block sepration
    colsep,
    rowsep,
    sepcolor = "white",
    sepwidth = c(0.05, 0.05),
    
    # cell labeling
    cellnote,
    notecex = 0.6,
    notecol = "cyan",
    na.color = par("bg"),
    
    # level trace
    trace = c("column", "row", "both", "none"),
    tracecol = "cyan",
    hline = median(breaks),
    vline = median(breaks),
    linecol = tracecol,
    
    # Row/Column Labeling
    margins = c(5, 5),
    ColSideColors,
    RowSideColors,
    cexRow = 0.6,
    cexCol = 0.6,
    labRow = NULL,
    labCol = NULL,
    srtRow = NULL,
    srtCol = NULL,
    adjRow = c(0,NA),
    adjCol = c(NA,0),
    offsetRow = 0.5,
    offsetCol = 0.5,
    colRow = NULL,
    colCol = NULL,
    
    # color key + density info
    key = TRUE,
    keysize = 1.5,
    density.info = c("histogram", "density", "none"),
    denscol = tracecol,
    symkey = any(x < 0, na.rm = TRUE) || symbreaks,
    densadj = 0.25,
    key.title = NULL,
    key.xlab = NULL,
    key.ylab = NULL,
    key.xtickfun = NULL,
    key.ytickfun = NULL,
    key.par = list(),
    
    # plot labels
    main = NULL,
    xlab = NULL,
    ylab = NULL,
    
    # plot layout
    lmat = NULL,
    lhei = NULL,
    lwid = NULL,
    
    # extras
    extrafun = NULL,
    ...)
}
\arguments{

  \item{x}{The input matrix.}
  \item{Rowv}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{Colv}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{distfun}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{hclustfun}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{dendrogram}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{reorderfun}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{symm}{Ignored.}
  \item{scale}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{na.rm}{Ignored.}
  \item{revC}{Ignored.}
  \item{add.expr}{Ignored.}
  \item{breaks}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{symbreaks}{Ignored.}
  \item{col}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{colsep}{Ignored.}
  \item{rowsep}{Ignored.}
  \item{sepcolor}{Ignored.}
  \item{sepwidth}{Ignored.}
  \item{cellnote}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{notecex}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{notecol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{na.color}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{trace}{'both' is reset to 'column', others are the same as in \code{\link[gplots]{heatmap.2}}.}
  \item{tracecol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{hline}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{vline}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{linecol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{margins}{Ignored.}
  \item{ColSideColors}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{RowSideColors}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{cexRow}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{cexCol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{labRow}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{labCol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{srtRow}{Ignored.}
  \item{srtCol}{Ignored.}
  \item{adjRow}{Ignored.}
  \item{adjCol}{Ignored.}
  \item{offsetRow}{Ignored.}
  \item{offsetCol}{Ignored.}
  \item{colRow}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{colCol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{key}{Always TRUE.}
  \item{keysize}{Ignored.}
  \item{density.info}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{denscol}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{symkey}{Ignored.}
  \item{densadj}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{key.title}{Always "Color Key".}
  \item{key.xlab}{Always "Value".}
  \item{key.ylab}{"Count" or "Density", depends on the value of \code{density.info}.}
  \item{key.xtickfun}{Ignored.}
  \item{key.ytickfun}{Ignored.}
  \item{key.par}{Ignored.}
  \item{main}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{xlab}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{ylab}{The same as in \code{\link[gplots]{heatmap.2}}.}
  \item{lmat}{Ignored.}
  \item{lhei}{Ignored.}
  \item{lwid}{Ignored.}
  \item{extrafun}{Ignored.}
  \item{...}{Other arguments passed to \code{\link{Heatmap}}.}

}
\details{
This function aims to execute \code{gplots::heatmap.2} code purely with ComplexHeatmap.
}
\value{
A \code{\link{Heatmap-class}} object.
}
\seealso{
\code{\link{compare_heatmap.2}} that compares heatmaps between \code{gplots::heatmap.2()} and \code{ComplexHeatmap::heatmap.2()}.
}
\examples{
# There is no example
NULL

}
