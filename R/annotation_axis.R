# == title
# Grob for annotation axis
#
# == param
# -at Break values.
# -labels Corresponding labels.
# -labels_rot Rotations of labels.
# -gp Graphic parameters.
# -side side of the axis to the annotation viewport.
# -facing Facing of the axis.
#
# == value
# A `grid::grob` object.
#
annotation_axis_grob = function(at = NULL, labels = at, labels_rot = 0, gp = gpar(), 
	side = "left", facing = "outside") {

	if(is.null(at)) {
		scale = current.viewport()$xscale
		at = pretty_breaks(scale)
		labels = at
	}
	if(is.null(labels)) {
		labels = at
	}

	if(side == "left" && facing == "inside") {
		gl = gList(
			linesGrob(unit(c(0, 0), "npc"), unit(c(0, 1), "npc"), gp = gp),
			segmentsGrob(unit(1, "mm"), at, unit(0, "npc"), at, 
				default.units = "native", gp = gp),
			textGrob(labels, unit(2, "mm"), at, default.units = "native",
				rot = 0, just = "left", gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = grobWidth(gl[[3]]) + unit(2, "mm")
		h = unit(1, "npc")
		attr(gb, "width") = w
		attr(gb, "height") = h
	} else if(side == "left" && facing == "outside") {
		gl = gList(
			linesGrob(unit(c(0, 0), "npc"), unit(c(0, 1), "npc"), gp = gp),
			segmentsGrob(unit(-1, "mm"), at, unit(0, "npc"), at, 
				default.units = "native", gp = gp),
			textGrob(labels, unit(-2, "mm"), at, default.units = "native",
				rot = 0, just = "right", gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = grobWidth(gl[[3]]) + unit(2, "mm")
		h = unit(1, "npc")
		attr(gb, "width") = w
		attr(gb, "height") = h
	} else if(side == "right" && facing == "inside") {
		gl = gList(
			linesGrob(unit(c(1, 1), "npc"), unit(c(0, 1), "npc"), gp = gp),
			segmentsGrob(unit(1, "npc") - unit(1, "mm"), at, unit(1, "npc"), at, 
				default.units = "native", gp = gp),
			textGrob(labels, unit(1, "npc") - unit(2, "mm"), at, default.units = "native",
				rot = 0, just = "right", gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = grobWidth(gl[[3]]) + unit(2, "mm")
		h = unit(1, "npc")
		attr(gb, "width") = w
		attr(gb, "height") = h
	} else if(side == "right" && facing == "outside") {
		gl = gList(
			linesGrob(unit(c(1, 1), "npc"), unit(c(0, 1), "npc"), gp = gp),
			segmentsGrob(unit(1, "npc") + unit(1, "mm"), at, unit(1, "npc"), at, 
				default.units = "native", gp = gp),
			textGrob(labels, unit(1, "npc") + unit(2, "mm"), at, default.units = "native",
				rot = 0, just = "left", gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = grobWidth(gl[[3]]) + unit(2, "mm")
		h = unit(1, "npc")
		attr(gb, "width") = w
		attr(gb, "height") = h
	}

	if(side == "top" && facing == "inside") {
		gl = gList(
			linesGrob(unit(c(0, 1), "npc"), unit(c(1, 1), "npc"), gp = gp),
			segmentsGrob(at, unit(1, "npc") - unit(1, "mm"), at, unit(1, "npc"), 
				default.units = "native", gp = gp),
			textGrob(labels, at, unit(1, "npc") - unit(2, "mm"), default.units = "native",
				rot = labels_rot, just = ifelse(labels_rot == 0, "top", "right"), gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = unit(1, "npc")
		h = grobHeight(gl[[3]]) + unit(2, "mm")
		attr(gb, "width") = w
		attr(gb, "height") = h
	} else if(side == "top" && facing == "outside") {
		gl = gList(
			linesGrob(unit(c(0, 1), "npc"), unit(c(1, 1), "npc"), gp = gp),
			segmentsGrob(at, unit(1, "npc") + unit(1, "mm"), at, unit(1, "npc"), 
				default.units = "native", gp = gp),
			textGrob(labels, at, unit(1, "npc") + unit(2, "mm"), default.units = "native",
				rot = labels_rot, just = ifelse(labels_rot == 0, "bottom", "left"), gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = unit(1, "npc")
		h = grobHeight(gl[[3]]) + unit(2, "mm")
		attr(gb, "width") = w
		attr(gb, "height") = h
	} else if(side == "bottom" && facing == "inside") {
		gl = gList(
			linesGrob(unit(c(0, 1), "npc"), unit(c(0, 0), "npc"), gp = gp),
			segmentsGrob(at, unit(1, "mm"), at, unit(0, "npc"), 
				default.units = "native", gp = gp),
			textGrob(labels, at, unit(2, "mm"), default.units = "native",
				rot = labels_rot, just = ifelse(labels_rot == 0, "bottom", "left"), gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = unit(1, "npc")
		h = grobHeight(gl[[3]]) + unit(2, "mm")
		attr(gb, "width") = w
		attr(gb, "height") = h
	} else if(side == "bottom" && facing == "outside") {
		gl = gList(
			linesGrob(unit(c(0, 1), "npc"), unit(c(0, 0), "npc"), gp = gp),
			segmentsGrob(at, unit(-1, "mm"), at, unit(0, "npc"), 
				default.units = "native", gp = gp),
			textGrob(labels, at, unit(-2, "mm"), default.units = "native",
				rot = labels_rot, just = ifelse(labels_rot == 0, "top", "right"), gp = gp)
		)
		gb = gTree(children = gl, cl = "annotation_axis")
		w = unit(1, "npc")
		h = grobHeight(gl[[3]]) + unit(2, "mm")
		attr(gb, "width") = w
		attr(gb, "height") = h
	}  
	return(gb)
}


# == title
# Grob width for annotation_axis
#
# == param
# -x legend body
#
widthDetails.annotation_axis = function(x) {
	attr(x, "width")
}

# == title
# Grob width for annotation_axis
#
# == param
# -x legend body
#
heightDetails.annotation_axis = function(x) {
	attr(x, "height")
}


pretty_breaks = function(x) {
	at = pretty(x, n = 3)
	at = at[at >= x[1] & at <= x[2]]
	at
}
