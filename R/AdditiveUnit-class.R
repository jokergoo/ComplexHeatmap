
# == title
# An internal class
#
# == detail
# This class is a super class for `Heatmap-class`, `HeatmapList-class` and `HeatmapAnnotation-class` classes.
# It is only designed for ``+`` generic method.
#
AdditiveUnit = setClass("AdditiveUnit")

# == title
# Constructor method for AdditiveUnit class
#
# == param
# -... arguments.
#
# == details
# This method is not used in the package.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
AdditiveUnit = function(...) {
    new("AdditiveUnit", ...)
}


# == title
# Add heatmaps or row annotations to a heatmap list
#
# == param
# -x a `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
# -y a `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
#
# == detail
# It is only a shortcut function. It actually calls `add_heatmap,Heatmap-method`, `add_heatmap,HeatmapList-method`
# or `add_heatmap,HeatmapAnnotation-method` depending on the class of the input objects.
#
# The `HeatmapAnnotation-class` object to be added should only be row annotation.
#
# == value
# A `HeatmapList-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
"+.AdditiveUnit" = function(x, y) {
    if(inherits(x, "HeatmapAnnotation")) {
    	if(x@which != "row") {
    		stop("You should specify `which` to `row` if you add a HeatmapAnnotation which shows row annotations.")
    	}
    }
    if(inherits(y, "HeatmapAnnotation")) {
    	if(y@which != "row") {
    		stop("You should specify `which` to `row` if you add a HeatmapAnnotation which shows row annotations.")
    	}
    }
    add_heatmap(x, y)
}
