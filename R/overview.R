# == title (package:ComplexHeatmap)
# Make complex heatmaps
#
# == details
# This package aims to provide a simple and flexible way to arrange
# multiple heatmaps as well as flexible annotation graphics.
#
# The package is implemented in an object-oriented way. 
# The heatmap lists are abstracted into several classes.
#
# - `Heatmap-class`: a single heatmap containing heatmap body, row/column names, titles, dendrograms and annotations.
# - `HeatmapList-class`: a list of heatmaps and annotations.
# - `HeatmapAnnotation-class`: a list of row/column annotations.
#
# There are also several internal classes:
#
# - `SingleAnnotation-class`: a single row annotation or column annotation.
# - `ColorMapping-class`: mapping from values to colors.
# - `AnnotationFunction-class`: construct an annotation function which allows subsetting.
#
# Following two high-level functions take use of functionality of complex heatmaps:
#
# - `oncoPrint`: oncoPrint plot which visualize genomic alterations in a set of genes.
# - `densityHeatmap`: use heatmaps to visualize density distributions.
#
# The complete reference of ComplexHeatmap package is available at http://jokergoo.github.io/ComplexHeatmap-reference/book.
#