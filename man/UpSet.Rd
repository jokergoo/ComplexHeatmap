\name{UpSet}
\alias{UpSet}
\title{
Make the UpSet plot
}
\description{
Make the UpSet plot
}
\usage{
UpSet(m,
    comb_col = "black",
    pt_size = unit(3, "mm"), lwd = 2,
    bg_col = "#F0F0F0", bg_pt_col = "#CCCCCC",
    set_order = order(set_size(m), decreasing = TRUE),
    comb_order = if(attr(m, "param")$set_on_rows) {
    order.comb_mat(m[set_order, ], decreasing = TRUE)
    } else {
    order.comb_mat(m[, set_order], decreasing = TRUE)
    },
    top_annotation = upset_top_annotation(m),
    right_annotation = upset_right_annotation(m),
    left_annotation = NULL,
    row_names_side = "left",
    ...)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}. The matrix can be transposed to switch the position of sets and combination sets.}
  \item{comb_col}{The color for the dots representing combination sets.}
  \item{pt_size}{The point size for the dots representing combination sets.}
  \item{lwd}{The line width for the combination sets.}
  \item{bg_col}{Color for the background rectangles.}
  \item{bg_pt_col}{Color for the dots representing the set is not selected.}
  \item{set_order}{The order of sets.}
  \item{comb_order}{The order of combination sets.}
  \item{top_annotation}{A \code{\link{HeatmapAnnotation}} object on top of the combination matrix.}
  \item{left_annotation}{A \code{\link{HeatmapAnnotation}} object on top of the combination matrix.}
  \item{right_annotation}{A \code{\link{HeatmapAnnotation}} object on the right of the combination matrix.}
  \item{row_names_side}{The side of row names.}
  \item{...}{Other arguments passed to \code{\link{Heatmap}}.}

}
\details{
By default, the sets are on rows and combination sets are on columns. The positions of the
two types of sets can be switched by transposing the matrix.

When sets are on rows, the default top annotation is the barplot showing the size of each
combination sets and the default right annotation is the barplot showing the size of the sets.
The annotations are simply constructed by \code{\link{HeatmapAnnotation}} and \code{\link{anno_barplot}} with some
parameters pre-set. Users can check the source code of \code{\link{upset_top_annotation}} and
\code{\link{upset_right_annotation}} to find out how the annotations are defined.

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
UpSet(m, comb_col = c(rep(2, 3), rep(3, 3), 1))

# compare two UpSet plots
set.seed(123)
lt1 = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m1 = make_comb_mat(lt1)
set.seed(456)
lt2 = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m2 = make_comb_mat(lt2)

max1 = max(c(set_size(m1), set_size(m2)))
max2 = max(c(comb_size(m1), comb_size(m2)))

UpSet(m1, top_annotation = upset_top_annotation(m1, ylim = c(0, max2)),
    right_annotation = upset_right_annotation(m1, ylim = c(0, max1)),
    column_title = "UpSet1") +
UpSet(m2, top_annotation = upset_top_annotation(m2, ylim = c(0, max2)),
    right_annotation = upset_right_annotation(m2, ylim = c(0, max1)),
    column_title = "UpSet2")
}
