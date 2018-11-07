.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Bioconductor page: http://bioconductor.org/packages/ComplexHeatmap/
Github page: https://github.com/jokergoo/ComplexHeatmap
Documentation: http://jokergoo.github.io/ComplexHeatmap-reference

If you use it in published research, please cite:
Gu, Z. Complex heatmaps reveal patterns and correlations in multidimensional 
  genomic data. Bioinformatics 2016.
========================================

This version is a major update of the package. The major new features are:

1. Support splitting heatmaps by columns.
2. Support concatenating heatmaps/annotations vertically.
3. Provide more types of heatmap annotations.

Note this version is not 100% compatible with the older versions (< 1.99.0).
Please check by `vignette('difference_to_old_versions', package = 'ComplexHeatmap')`.

Above messages will be removed in the future.
")	

    packageStartupMessage(msg)
}
