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

This message can be suppressed by:
  suppressPackageStartupMessages(library(ComplexHeatmap))
========================================
")	

  	if("package:pheatmap" %in% search()) {
  		msg = paste0(msg, 
"! pheatmap() has been masked by ComplexHeatmap::pheatmap(). 90% of the arguments
   in the original pheatmap() are identically supported in the new function. You 
   can still use the original function by explicitly calling pheatmap::pheatmap().
")
  	}

    packageStartupMessage(msg)
}

rv = R.Version()

if(getRversion() >= "4.0.0" && as.numeric(rv$`svn rev`) >= 77889) {
    unitType = get("unitType", envir = asNamespace("grid"))
} else {
	unitType = function(x, recurse = TRUE) attr(x, "unit")
}

