
# == title
# An internal class
#
# == detail
# This class is a super class for `Heatmap-class`, `HeatmapList-class` and `HeatmapAnnotation-class` classes.
# It is only designed for ``+`` generic method so that above three classes can be appended to each other.
#
AdditiveUnit = setClass("AdditiveUnit")

# == title
# Constructor method for AdditiveUnit class
#
# == param
# -... black hole arguments.
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
# == example
# # no example for this function
# NULL
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
# It is only a helper function. It actually calls `add_heatmap,Heatmap-method`, `add_heatmap,HeatmapList-method`
# or `add_heatmap,HeatmapAnnotation-method` depending on the class of the input objects.
#
# The `HeatmapAnnotation-class` object to be added should only be row annotations.
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
    		stop("You should specify `which` to `row` or use `rowAnnotation()` directly if you want to add row annotations.")
    	}
    }
    if(inherits(y, "HeatmapAnnotation")) {
    	if(y@which != "row") {
            stop("You should specify `which` to `row` or use `rowAnnotation()` directly if you want to add row annotations.")
    	}
    }
    if(is.null(x)) {
        ht_list = new("HeatmapList")
        add_heatmap(ht_list, y)
    } else if(is.null(y)) {
        ht_list = new("HeatmapList")
        add_heatmap(ht_list, x)
    } else {
        add_heatmap(x, y)
    }
}
