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

    packageStartupMessage(msg)
}

if(getRversion() >= "4.0.0") {
    unitType = get("unitType", envir = asNamespace("grid"))
} else {
	unitType = function(x) attr(x, "unit")
}


"[[.unit" = function(x, index) {
    as.numeric(x[index])
}

# value is a pure number
"[[<-.unit" = function(x, index, value) {
	ut = unitType(x[index])
    x[index] = unit(value, ut)
    x
}

