# == title (package:ComplexHeatmap)
# Make complex heatmaps
#
# == details
# This package aims to provide a simple and flexible way to arrange
# multiple heatmaps as well as flexible annotation graphics.
#
# The package is implemented in an object-oriented way. 
# Components of heatmap lists are abstracted into several classes.
#
# - `Heatmap-class`: a single heatmap containing heatmap body, row/column names, titles, dendrograms and column annotations.
# - `HeatmapList-class`: a list of heatmaps and row/column annotations.
# - `HeatmapAnnotation-class`: a list of row/column annotations.
#
# There are also several internal classes:
#
# - `SingleAnnotation-class`: a single row annotation or column annotation.
# - `ColorMapping-class`: mapping from values to colors.
# - `AnnotationFunction-class`: construct an annotation function which allows subsetting.
#
# For plotting one single heatmap, please go to the documentation page of `Heatmap`.
# For plotting multiple heatmaps, please go to `HeatmapList-class`, ``+.AdditiveUnit`` and ``\%v\%.AdditiveUnit``.
#
# You can refer to the ComplexHeatmap Complete Reference for all the information of this package ().
