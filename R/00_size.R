
width = function (x, ...) {
	UseMethod("width", x)
}

height = function (x, ...) {
	UseMethod("height", x)
}

size = function (x, ...) {
	UseMethod("size", x)
}

"width<-" = function (x, ..., value) {
	UseMethod("width<-", x)
}

"height<-" = function (x, ..., value) {
	UseMethod("height<-", x)
}

"size<-" = function (x, ..., value) {
	UseMethod("size<-", x)
}

# == title
# Width of the AnnotationFunction Object
#
# == param
# -x A `AnnotationFunction-class` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
# == example
# anno = anno_points(1:10)
# ComplexHeatmap:::width(anno)
# anno = anno_points(1:10, which = "row")
# ComplexHeatmap:::width(anno)
width.AnnotationFunction = function(x, ...) {
	x@width
}

# == title
# Assign the Width to the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -... Other arguments.
# -value A `grid::unit` object.
#
# == detail
# Internally used.
#
"width<-.AnnotationFunction" = function(x, ..., value) {
	x@width = value
	x
}

# == title
# Height of the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
# == example
# anno = anno_points(1:10)
# ComplexHeatmap:::height(anno)
# anno = anno_points(1:10, which = "row")
# ComplexHeatmap:::height(anno)
height.AnnotationFunction = function(x, ...) {
	x@height
}

# == title
# Assign the Height to the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
"height<-.AnnotationFunction" = function(x, ..., value) {
	x@height = value
	x
}

# == title
# Size of the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -... Other arguments.
#
# == detail
# It returns the width if it is a row annotation and the height if it is a column annotation.
#
# Internally used.
#
# == example
# anno = anno_points(1:10)
# ComplexHeatmap:::size(anno)
# anno = anno_points(1:10, which = "row")
# ComplexHeatmap:::size(anno)
size.AnnotationFunction = function(x, ...) {
	if(x@which == "row") {
		x@width
	} else {
		x@height
	}
}

# == title
# Assign the Size to the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# It assigns to the width if it is a row annotation and the height if it is a column annotation.
#
# Internally used.
#
# == example
# anno = anno_points(1:10)
# ComplexHeatmap:::size(anno) = unit(4, "cm")
# ComplexHeatmap:::size(anno)
"size<-.AnnotationFunction" = function(x, ..., value) {
	if(x@which == "row") {
		x@width = value
	} else {
		x@height = value
	}
	x
}



# == title
# Width of the SingleAnnotation Object
#
# == param
# -x The `SingleAnnotation-class` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
width.SingleAnnotation = function(x, ...) {
    x@width
}

# == title
# Assign the Width to the SingleAnnotation Object
#
# == param
# -x The `SingleAnnotation-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
"width<-.SingleAnnotation" = function(x, ..., value) {
    x@width = value
    if(inherits(x@fun, "AnnotationFunction")) {
        width(x@fun) = value
    }
    x
}

# == title
# Height of the SingleAnnotation object
#
# == param
# -x The `SingleAnnotation-class` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
height.SingleAnnotation = function(x, ...) {
    x@height
}

# == title
# Assign the Height to the SingleAnnotation Object
#
# == param
# -x The `SingleAnnotation-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
"height<-.SingleAnnotation" = function(x, ..., value) {
    x@height = value
    if(inherits(x@fun, "AnnotationFunction")) {
        height(x@fun) = value
    }
    x
}

# == title
# Size of the SingleAnnotation Object
#
# == param
# -x The `SingleAnnotation-class` object.
# -... Other arguments.
#
# == detail
# It returns the width if it is a row annotation and the height if it is a column annotation.
#
# Internally used.
#
size.SingleAnnotation = function(x, ...) {
    if(x@which == "row") {
        x@width
    } else {
        x@height
    }
}

# == title
# Assign the Size to the SingleAnnotation Object
#
# == param
# -x The `SingleAnnotation-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# It assigns to the width if it is a row annotation and the height if it is a column annotation.
#
# Internally used.
#
"size<-.SingleAnnotation" = function(x, ..., value) {
    if(x@which == "row") {
        width(x) = value
    } else {
        height(x) = value
    }
    x
}



# == title
# Width of the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
width.HeatmapAnnotation = function(x, ...) {
    x@width
}

# == title
# Assign the Width to the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
"width<-.HeatmapAnnotation" = function(x, ..., value) {

    if(x@which == "column") {
    	x@width = value
    	for(i in seq_along(x@anno_list)) {
    		width(x@anno_list[[i]]) = value
    	}
    } else {
    	x = re_size(x, width = value)
    }
    x
}


# == title
# Height of the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
height.HeatmapAnnotation = function(x, ...) {
    x@height
}

# == title
# Assign the Height to the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# Internally used.
#
"height<-.HeatmapAnnotation" = function(x, ..., value) {

    if(x@which == "row") {
    	x@height = value
    	for(i in seq_along(x@anno_list)) {
    		height(x@anno_list[[i]]) = height
    	}
    } else {
    	x = re_size(x, height = value)
    }
    x
}

# == title
# Size of the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -... Other arguments.
#
# == detail
# It returns the width if it is a row annotation and the height if it is a column annotation.
#
# Internally used.
#
size.HeatmapAnnotation = function(x, ...) {
    if(x@which == "row") {
        x@width
    } else {
        x@height
    }
}

# == title
# Assign the Size to the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
#
# == detail
# It assigns the width if it is a row annotation and the height if it is a column annotation.
#
# Internally used.
#
"size<-.HeatmapAnnotation" = function(x, ..., value) {
    if(x@which == "row") {
        width(x) = value
    } else {
        height(x) = value
    }
    x
}


# == title
# Width of the Legends
#
# == param
# -x The `grid::grob` object returned by `Legend` or `packLegend`.
# -... Other arguments.
#
# == value
# The returned unit x is always in ``mm``.
#
# == example
# lgd = Legend(labels = 1:10, title = "foo", legend_gp = gpar(fill = "red"))
# ComplexHeatmap:::width(lgd)
#
width.Legends = function(x, ...) {
	s = attr(x@grob, "width")
	convertWidth(s, "mm")
}


# == title
# Height of the Legends
#
# == param
# -x The `grid::grob` object returned by `Legend` or `packLegend`.
# -... Other arguments.
#
# == value
# The returned unit x is always in ``mm``.
#
# == example
# lgd = Legend(labels = 1:10, title = "foo", legend_gp = gpar(fill = "red"))
# ComplexHeatmap:::height(lgd)
#
height.Legends = function(x, ...) {
	s = attr(x@grob, "height")
	convertHeight(s, "mm")
}


# == title
# Width of the Heatmap List
#
# == param
# -x The `HeatmapList-class` object returned by `draw,HeatmapList-method`.
# -... Other arguments.
#
width.HeatmapList = function(x, ...) {
    if(x@layout$initialized) {
        x@ht_list_param$width
    } else {
        stop_wrap("width() can only be applied to the heatmap list object returned by draw().")
    }
}


# == title
# Height of the Heatmap List
#
# == param
# -x The `HeatmapList-class` object returned by `draw,HeatmapList-method`.
# -... Other arguments.
#
height.HeatmapList = function(x, ...) {
    if(x@layout$initialized) {
        x@ht_list_param$height
    } else {
        stop_wrap("height() can only be applied to the heatmap list object returned by draw().")
    }
}


# == title
# Width of the Heatmap
#
# == param
# -x The `HeatmapList-class` object returned by `draw,Heatmap-method`.
# -... Other arguments.
#
width.Heatmap = function(x, ...) {
    stop_wrap("width() can only be applied to the heatmap object returned by draw().")
}


# == title
# Height of the Heatmap
#
# == param
# -x The `HeatmapList-class` object returned by `draw,Heatmap-method`.
# -... Other arguments.
#
height.Heatmap = function(x, ...) {
    stop_wrap("height() can only be applied to the heatmap object returned by draw().")
}

