
# == title
# An internal class
#
# == detail
# This class is a super class for `Heatmap-class`, `HeatmapList-class` and `HeatmapAnnotation-class` classes.
# It is only designed for ``+`` generic method so that above three classes can be appended to each other.
#
AdditiveUnit = setClass("AdditiveUnit")

# == title
# Constructor Method for AdditiveUnit Class
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
AdditiveUnit = function(...) {
    new("AdditiveUnit", ...)
}


# == title
# Horizontally Add Heatmaps or Annotations to a Heatmap List
#
# == param
# -x A `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
# -y A `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
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
# == seealso
# `\%v\%` operator is used for vertical heatmap list.
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
        ht_list@direction = "horizontal"
        add_heatmap(ht_list, y)
    } else if(is.null(y)) {
        ht_list = new("HeatmapList")
        ht_list@direction = "horizontal"
        add_heatmap(ht_list, x)
    } else {
        add_heatmap(x, y)
    }
}


# == title
# Vertically Add Heatmaps or Annotations to a Heatmap List
#
# == param
# -x A `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
# -y A `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
#
# == detail
# It is only a helper function. It actually calls `add_heatmap,Heatmap-method`, `add_heatmap,HeatmapList-method`
# or `add_heatmap,HeatmapAnnotation-method` depending on the class of the input objects.
#
# The `HeatmapAnnotation-class` object to be added should only be column annotations.
#
# == value
# A `HeatmapList-class` object.
#
# == seealso
# `+.AdditiveUnit` operator is used for vertical heatmap list.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
"%v%" = function(x, y) {
    if(inherits(x, "HeatmapAnnotation")) {
        if(x@which != "column") {
            stop("You should specify `which` to `column` or use `columnAnnotation()` directly if you want to add column annotations vertically.")
        }
    }
    if(inherits(y, "HeatmapAnnotation")) {
        if(y@which != "column") {
            stop("You should specify `which` to `column` or use `columnAnnotation()` directly if you want to add column annotations vertically.")
        }
    }
    if(is.null(x)) {
        ht_list = new("HeatmapList")
        ht_list@direction = "vertical"
        add_heatmap(ht_list, y, direction = "vertical")
    } else if(is.null(y)) {
        ht_list = new("HeatmapList")
        ht_list@direction = "vertical"
        add_heatmap(ht_list, x, direction = "vertical")
    } else {
        add_heatmap(x, y, direction = "vertical")
    }
}

