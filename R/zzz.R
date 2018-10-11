.onAttach = function(libname, pkgname) {
    version = packageDescription(pkgname, fields = "Version")

  	msg = paste0("========================================
", pkgname, " version ", version, "
Bioconductor page: http://bioconductor.org/packages/ComplexHeatmap/
Github page: https://github.com/jokergoo/ComplexHeatmap
Documentation: http://jokergoo.github.io/ComplexHeatmap_book

If you use it in published research, please cite:
Gu, Z. Complex heatmaps reveal patterns and correlations in multidimensional 
  genomic data. Bioinformatics 2016.
========================================
")	

    packageStartupMessage(msg)
}
