\name{anno_numeric}
\alias{anno_numeric}
\title{
Numeric labels annotation
}
\description{
Numeric labels annotation
}
\usage{
anno_numeric(x, rg = range(x), labels_gp = gpar(), x_convert = NULL,
    labels_format = NULL, labels_offset = unit(4, "pt"),
    bg_gp = gpar(fill = "#8080FF", col = "#8080FF"),
    bar_width = unit(1, "npc") - unit(4, "pt"),
    round_corners = TRUE, r = unit(0.05, "snpc"),
    which = c("row", "column"), align_to = "left", width = NULL)
}
\arguments{

  \item{x}{A vector of numeric values.}
  \item{rg}{Range. A numeric vector of length two.}
  \item{labels_gp}{Graphics parameters for labels.}
  \item{x_convert}{A function applied on \code{x}. E.g. when \code{x} contains p-values, to map \code{x} to the heights of bars, a transformation of \code{-log10(x)}  is normally applied.}
  \item{labels_format}{A function applied on \code{x}. E.g., when \code{x} is a numeric, \code{labels_format} can be set to \code{function(x) sprintf("\%.2f", x)}.}
  \item{labels_offset}{Offset of labels to the left or right of bars.}
  \item{bg_gp}{Graphics parameters for the background bars.}
  \item{bar_width}{Width of bars. Note it corresponds to the vertical direction.}
  \item{round_corners}{Whether to draw bars with round corners?}
  \item{r}{Radius of the round corners.}
  \item{which}{Row or column. Currently it only supports row annotation.}
  \item{align_to}{Which side bars as well as the labels are aligned to. Values can be "left" or "right". If \code{x} contains both positive and negative values, \code{align_to} can also be set to 0 so that bars are aligned to \code{pos = 0}.}
  \item{width}{Width of the annotation.}

}
\examples{
m = matrix(rnorm(100), 10)
x = rnorm(10)
Heatmap(m, right_annotation = rowAnnotation(numeric = anno_numeric(x)))
}
