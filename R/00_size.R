
# == title
# Generic method for width
#
# == param
# -x x
# -... other arguments
#
width = function (x, ...) {
	UseMethod("width", x)
}

# == title
# Generic method for height
#
# == param
# -x x
# -... other arguments
#
height = function (x, ...) {
	UseMethod("height", x)
}

# == title
# Generic method for size
#
# == param
# -x x
# -... other arguments
#
size = function (x, ...) {
	UseMethod("size", x)
}

# == title
# Generic assignment method for width
#
# == param
# -x x
# -value value
# -... other arguments
#
"width<-" = function (x, ..., value) {
	UseMethod("width<-", x)
}

# == title
# Generic assignment method for height
#
# == param
# -x x
# -value value
# -... other arguments
#
"height<-" = function (x, ..., value) {
	UseMethod("height<-", x)
}

# == title
# Generic assignment method for size
#
# == param
# -x x
# -value value
# -... other arguments
#
"size<-" = function (x, ..., value) {
	UseMethod("size<-", x)
}

# == title
# Width of the AnnotationFunction x
#
# == param
# -x The `AnnotationFunction-class` x.
# -... other arguments
#
# == example
# anno = anno_points(1:10)
# width(anno)
# anno = anno_points(1:10, which = "row")
# width(anno)
width.AnnotationFunction = function(x, ...) {
	x@width
}

# == title
# Assign the Width to the AnnotationFunction x
#
# == param
# -x The `AnnotationFunction-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
"width<-.AnnotationFunction" = function(x, ..., value) {
	x@width = value
	x
}

# == title
# Height of the AnnotationFunction x
#
# == param
# -x The `AnnotationFunction-class` x.
# -... other arguments
#
# == example
# anno = anno_points(1:10)
# height(anno)
# anno = anno_points(1:10, which = "row")
# height(anno)
height.AnnotationFunction = function(x, ...) {
	x@height
}

# == title
# Assign the Height to the AnnotationFunction x
#
# == param
# -x The `AnnotationFunction-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
"height<-.AnnotationFunction" = function(x, ..., value) {
	x@height = value
	x
}

# == title
# Size of the AnnotationFunction x
#
# == param
# -x The `AnnotationFunction-class` x.
# -... other arguments
#
# == detail
# It returns the width if it is a row annotation and the height if it is a column annotation.
#
# == example
# anno = anno_points(1:10)
# size(anno)
# anno = anno_points(1:10, which = "row")
# size(anno)
size.AnnotationFunction = function(x, ...) {
	if(x@which == "row") {
		x@width
	} else {
		x@height
	}
}

# == title
# Assign the Size to the AnnotationFunction x
#
# == param
# -x The `AnnotationFunction-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
# == detail
# It assigns the width if it is a row annotation and the height if it is a column annotation.
#
# == example
# anno = anno_points(1:10)
# size(anno) = unit(4, "cm")
# size(anno)
"size<-.AnnotationFunction" = function(x, ..., value) {
	if(x@which == "row") {
		x@width = value
	} else {
		x@height = value
	}
	x
}



# == title
# Width of the SingleAnnotation x
#
# == param
# -x The `SingleAnnotation-class` x.
# -... other arguments
#
width.SingleAnnotation = function(x, ...) {
    x@width
}

# == title
# Assign the Width to the SingleAnnotation x
#
# == param
# -x The `SingleAnnotation-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
"width<-.SingleAnnotation" = function(x, ..., value) {
    x@width = value
    if(inherits(x@fun, "AnnotationFunction")) {
        width(x@fun) = value
    }
    x
}

# == title
# Height of the SingleAnnotation x
#
# == param
# -x The `SingleAnnotation-class` x.
# -... other arguments
#
height.SingleAnnotation = function(x, ...) {
    x@height
}

# == title
# Assign the Height to the SingleAnnotation x
#
# == param
# -x The `SingleAnnotation-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
"height<-.SingleAnnotation" = function(x, ..., value) {
    x@height = value
    if(inherits(x@fun, "AnnotationFunction")) {
        height(x@fun) = value
    }
    x
}

# == title
# Size of the SingleAnnotation x
#
# == param
# -x The `SingleAnnotation-class` x.
# -... other arguments
#
# == detail
# It returns the width if it is a row annotation and the height if it is a column annotation.
#
size.SingleAnnotation = function(x, ...) {
    if(x@which == "row") {
        x@width
    } else {
        x@height
    }
}

# == title
# Assign the Size of the SingleAnnotation x
#
# == param
# -x The `SingleAnnotation-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
# == detail
# It assigns the width if it is a row annotation and the height if it is a column annotation.
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
# Width of the HeatmapAnnotation x
#
# == param
# -x The `HeatmapAnnotation-class` x.
# -... other arguments
#
width.HeatmapAnnotation = function(x, ...) {
    x@width
}

# == title
# Assign the Width to the HeatmapAnnotation x
#
# == param
# -x The `HeatmapAnnotation-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
"width<-.HeatmapAnnotation" = function(x, ..., value) {

    if(x@which == "column") {
    	x@width = value
    	for(i in seq_along(x@anno_list)) {
    		width(x@anno_list[[i]]) = value
    	}
    } else {
    	x = resize(x, width = value)
    }
    x
}


# == title
# Height of the HeatmapAnnotation x
#
# == param
# -x The `HeatmapAnnotation-class` x.
# -... other arguments
#
height.HeatmapAnnotation = function(x, ...) {
    x@height
}

# == title
# Assign the Height to the HeatmapAnnotation x
#
# == param
# -x The `HeatmapAnnotation-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
"height<-.HeatmapAnnotation" = function(x, ..., value) {

    if(x@which == "row") {
    	x@height = value
    	for(i in seq_along(x@anno_list)) {
    		height(x@anno_list[[i]]) = height
    	}
    } else {
    	x = resize(x, height = value)
    }
    x
}

# == title
# Size of the HeatmapAnnotation x
#
# == param
# -x The `HeatmapAnnotation-class` x.
# -... other arguments
#
# == detail
# It returns the width if it is a row annotation and the height if it is a column annotation.
#
size.HeatmapAnnotation = function(x, ...) {
    if(x@which == "row") {
        x@width
    } else {
        x@height
    }
}

# == title
# Assign the Size to the HeatmapAnnotation x
#
# == param
# -x The `HeatmapAnnotation-class` x.
# -value A `grid::unit` x.
# -... other arguments
#
# == detail
# It assigns the width if it is a row annotation and the height if it is a column annotation.
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
# -x The `grid::grob` x returned by `Legend` or `packLegend`.
# -... other arguments
#
# == value
# The returned unit x is in ``mm``.
#
width.Legends = function(x, ...) {
	attr(x@grob, "width")
}

# == title
# Height of the Legends
#
# == param
# -x The `grid::grob` x returned by `Legend` or `packLegend`.
# -... other arguments
#
# == value
# The returned unit x is in ``mm``.
#
height.Legends = function(x, ...) {
	attr(x@grob, "height")
}

