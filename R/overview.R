# == title (package:ComplexHeatmap)
# Making complex heatmap
#
# == details
# This package aims to provide a simple and flexible way to arrange
# multiple heatmaps as well as self-defining annotation graphics.
#
# The package is implemented in an object-oriented way. 
# Components of heatmap lists are abstracted into several classes.
#
# - `Heatmap-class`: a single heatmap containing heatmap body, row/column names, titles, dendrograms and column annotations.
# - `HeatmapList-class`: a list of heatmaps and row annotations.
# - `HeatmapAnnotation-class`: a list of row annotations or column annotations.
#
# There are also several internal classes:
#
# - `SingleAnnotation-class`: a single row annotation or column annotation.
# - `ColorMapping-class`: mapping from values to colors.
#
# For plotting one single heatmap, please go to the documentation page of `Heatmap`.
# For plotting multiple heatmaps, please go to `HeatmapList-class` and ``+.AdditiveUnit``.
#
# The vignette provides detailed explanation of how to use this package.
#