\name{UpSet}
\alias{UpSet}
\title{
Make the UpSet plot
}
\description{
Make the UpSet plot
}
\usage{
UpSet(m, set_order = order(set_size(m), decreasing = TRUE),
    comb_order = order(comb_size(m), decreasing = TRUE),
    top_annotation = default_upset_top_annotation(m),
    right_annotation = default_upset_right_annotation(m),
    ...)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}. The matrix can be transposed to switch the position of sets and combination sets.}
  \item{set_order}{The order of sets.}
  \item{comb_order}{The order of combination sets.}
  \item{top_annotation}{A \code{\link{HeatmapAnnotation}} object on top of the combination matrix.}
  \item{right_annotation}{A \code{\link{HeatmapAnnotation}} object on the right of the combination matrix.}
  \item{...}{Other arguments passed to \code{\link{Heatmap}}.}

}
\details{
BY default, the sets are on rows and combination sets are on columns. The positions of the
two types of sets can be switched by transposing the matrix.

When sets are on rows, the default top annotation is the barplot showing the size of each
combination sets and the default right annotation is the barplot showing the size of the sets.
The annotations are simply constructed by \code{\link{HeatmapAnnotation}} and \code{\link{anno_barplot}} with some
parameters pre-set. Users can check the source code of \code{\link{default_upset_top_annotation}} and
\code{\link{default_upset_right_annotation}} to find out how the annotations are defined.

To change or to add annotations, users just need to define a new \code{\link{HeatmapAnnotation}} object.
E.g. if we want to change the side of the axis and name on top annotation:

  \preformatted{
    Upset(..., top_annotation = 
        HeatmapAnnotation(
           "Intersection size" = anno_barplot(
               comb_size(m), 
               border = FALSE, 
               gp = gpar(fill = "black"), 
               height = unit(2, "cm"),
               axis_param = list(side = "right")
           ), 
           annotation_name_side = "right", 
           annotation_name_rot = 0)
    )  }

To add more annotations on top, users just add it in \code{\link{HeatmapAnnotation}}:

  \preformatted{
    Upset(..., top_annotation = 
        HeatmapAnnotation(
           "Intersection size" = anno_barplot(
               comb_size(m), 
               border = FALSE, 
               gp = gpar(fill = "black"), 
               height = unit(2, "cm"),
               axis_param = list(side = "right")
           ), 
           "anno1" = anno_points(...),
           "anno2" = some_vector, 
           annotation_name_side = "right", 
           annotation_name_rot = 0)
    )  }

And so is for the right annotations.

\code{\link{UpSet}} returns a \code{\link{Heatmap-class}} object, which means, you can add it with other heatmaps and annotations
by \code{+} or \code{\link[=pct_v_pct]{\%v\%}}.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
	      b = sample(letters, 15),
	      c = sample(letters, 20))
m = make_comb_mat(lt)
UpSet(m)
UpSet(t(m))

m = make_comb_mat(lt, mode = "union")
UpSet(m)
}
