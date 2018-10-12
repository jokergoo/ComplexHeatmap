
# == title
# Generic Method for width()
#
# == param
# -x An object.
# -... Other arguments.
#
width = function (x, ...) {
	UseMethod("width", x)
}

# == title
# Generic Method for height()
#
# == param
# -x An object.
# -... Other arguments.
#
height = function (x, ...) {
	UseMethod("height", x)
}

# == title
# Generic Method for size()
#
# == param
# -x An object.
# -... Other arguments.
#
size = function (x, ...) {
	UseMethod("size", x)
}

# == title
# Generic Assignment Method for width()
#
# == param
# -x An object.
# -value The value
# -... Other arguments.
#
"width<-" = function (x, ..., value) {
	UseMethod("width<-", x)
}

# == title
# Generic Assignment Method for height()
#
# == param
# -x An object.
# -value The value
# -... Other arguments.
#
"height<-" = function (x, ..., value) {
	UseMethod("height<-", x)
}

# == title
# Generic Assignment Method for size()
#
# == param
# -x An object.
# -value The value
# -... Other arguments.
#
"size<-" = function (x, ..., value) {
	UseMethod("size<-", x)
}

# == title
# Width of the AnnotationFunction Object
#
# == param
# -x A `AnnotationFunction-class` object.
# -... Other arguments
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
# Assign the Width to the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -... Other arguments.
# -value A `grid::unit` object.
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
# -... Other arguments
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
# Assign the Height to the AnnotationFunction Object
#
# == param
# -x The `AnnotationFunction-class` object.
# -value A `grid::unit` object.
# -... Other arguments.
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
# Width of the SingleAnnotation Object
#
# == param
# -x The `SingleAnnotation-class` object.
# -... Other arguments.
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
# Height of the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -... Other arguments.
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
# Size of the HeatmapAnnotation Object
#
# == param
# -x The `HeatmapAnnotation-class` object.
# -... Other arguments.
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
width.Legends = function(x, ...) {
	s = attr(x@grob, "width")
	s
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
height.Legends = function(x, ...) {
	s = attr(x@grob, "height")
	s
}

