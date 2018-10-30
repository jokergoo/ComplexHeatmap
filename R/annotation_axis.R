# == title
# Grob for Annotation Axis
#
# == param
# -at Break values. If it is not specified, it is inferred from data scale in current viewport.
# -labels Corresponding labels.
# -labels_rot Rotations of labels.
# -gp Graphic parameters.
# -side side of the axis of the annotation viewport.
# -facing Facing of the axis.
#
# == value
# A `grid::grob` object.
#
# == examples
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "left", facing = "outside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(x = unit(0, "npc"), width = grobWidth(gb), just = "right")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "left", facing = "inside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(x = unit(0, "npc"), width = grobWidth(gb), just = "left")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "right", facing = "outside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(x = unit(1, "npc"), width = grobWidth(gb), just = "left")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "right", facing = "inside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(x = unit(1, "npc"), width = grobWidth(gb), just = "right")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "top", facing = "outside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(y = unit(1, "npc"), height = grobHeight(gb), just = "bottom")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 90, 
#     side = "top", facing = "outside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(y = unit(1, "npc"), height = grobHeight(gb), just = "bottom")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 45, 
#     side = "top", facing = "outside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(y = unit(1, "npc"), height = grobHeight(gb), just = "bottom")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "top", facing = "inside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(y = unit(1, "npc"), height = grobHeight(gb), just = "top")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "bottom", facing = "outside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(y = unit(0, "npc"), height = grobHeight(gb), just = "top")
# popViewport()
#
# gb = annotation_axis_grob(at = 1:5, labels = month.name[1:5], labels_rot = 0, 
#     side = "bottom", facing = "inside")
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# grid.draw(gb)
# grid.rect(y = unit(0, "npc"), height = grobHeight(gb), just = "bottom")
# popViewport()
#
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 6), yscale = c(0, 6), width = 0.6, height = 0.6))
# gb = annotation_axis_grob(labels_rot = 0, side = "left", facing = "outside")
# grid.draw(gb)
# grid.rect(x = unit(0, "npc"), width = grobWidth(gb), just = "right")
# popViewport()
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
# Width for annotation_axis Grob
#
# == param
# -x The ``annotation_axis`` grob returned by `annotation_axis_grob`.
#
# == details
# The physical width of the grob can be get by ``convertWidth(grobWidth(axis_grob), "mm")``.
#
widthDetails.annotation_axis = function(x) {
	attr(x, "width")
}

# == title
# Height for annotation_axis Grob
#
# == param
# -x The ``annotation_axis`` grob returned by `annotation_axis_grob`.
#
# == details
# The physical height of the grob can be get by ``convertWidth(grobHeight(axis_grob), "mm")``.
#
heightDetails.annotation_axis = function(x) {
	attr(x, "height")
}


pretty_breaks = function(x) {
	at = pretty(x, n = 3)
	at = at[at >= x[1] & at <= x[2]]
	at
}

# == title
# Draw Annotation Axis
#
# == param
# -at Break values. If it is not specified, it is inferred from data scale in current viewport.
# -labels Corresponding labels.
# -labels_rot Rotations of labels.
# -gp Graphic parameters.
# -side side of the axis of the annotation viewport.
# -facing Facing of the axis.
#
# == details
# It uses `annotation_axis_grob` to construct the grob object, then use `grid::grid.draw`
# to draw the axis.
#
# == example
# # See examples in `annotation_axis_grob`
# NULL
#
grid.annotation_axis = function(at = NULL, labels = at, labels_rot = 0, gp = gpar(), 
	side = "left", facing = "outside") {
	grob = annotation_axis_grob(at = at, labels = labels, labels_rot = labels_rot, gp = gp,
		side = side, facing = facing)
	grid.draw(grob)
}