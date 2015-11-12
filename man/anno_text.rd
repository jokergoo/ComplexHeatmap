\name{anno_text}
\alias{anno_text}
\title{
Using text as annotation
}
\description{
Using text as annotation
}
\usage{
anno_text(x, which = c("column", "row"), gp = gpar(), rot = 0,
    just = NULL, offset = unit(0.5, "npc"))
}
\arguments{

  \item{x}{a vector of text}
  \item{which}{is the annotation a column annotation or a row annotation?}
  \item{gp}{graphic parameters.}
  \item{rot}{rotation of text}
  \item{just}{justification of text, pass to \code{\link[grid]{grid.text}}}
  \item{offset}{if it is a row annotation, \code{offset} corresponds to the x-coordinates of text. and if it is a column annotation, \code{offset} corresponds to the y-coordinates of text. The value should be a \code{\link[grid]{unit}} object.}

}
\value{
A graphic function which can be set in \code{\link{HeatmapAnnotation}} constructor method.
}
\author{
Zuguang Gu <z.gu@dkfz.de>
}
\examples{
mat = matrix(rnorm(100), 10)
colnames(mat) = letters[1:10]
rownames(mat) = LETTERS[1:10]
long_cn = do.call("paste0", rep(list(colnames(mat)), 4))  # just to construct long text
ha_rot_cn = HeatmapAnnotation(text = anno_text(long_cn, rot = 45, offset = unit(5, "mm")))
Heatmap(mat, name = "foo", top_annotation = ha_rot_cn, top_annotation_height = unit(1.2, "cm"))

}
