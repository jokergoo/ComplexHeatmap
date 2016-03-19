# == title
# Decorate the heatmap body
#
# == param
# -heatmap name of the heatmap which is set as ``name`` option in `Heatmap` function
# -code code that adds graphics in the selected heatmap body
# -slice index of row slices in the heatmap if it is split by rows
#
# == details
# There is a viewport for each row slice in each heatmap.
# This function contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
#
# If you know the number of rows and columns for that row slice, it is
# simple to calculate the position of every small grid in the row slice.
# E.g., the position for the grid in i^th row and j^th column is:
#
#     # assume nc is the number of columns 
#     # and nr is the number of rows in that row slice
#     unit((i-0.5)/nc, "npc")
#     unit((j-0.5)/nr, "npc")
#
#     # the width is
#     unit(1/nc, "npc")
#
#     # the height is
#     unit(1/nr, "npc")
#
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# set.seed(123)
# Heatmap(matrix(rnorm(100), 10), name = "mat")
# decorate_heatmap_body("mat", {
#     grid.circle(gp = gpar(fill = "#FF000080"))
# })
#
decorate_heatmap_body = function(heatmap, code, slice = 1) {

	if(is.null(slice)) {
		vp_name = paste0(heatmap, "_heatmap_body_", 1)
		seekViewport(vp_name)
		upViewport()
	} else {
		vp_name = paste0(heatmap, "_heatmap_body_", slice)
		seekViewport(vp_name)
	}

	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)

	seekViewport("global")
}

# == title
# Decorate the heatmap dendrogram
#
# == param
# -heatmap name of the heatmap
# -code code that adds graphics in the selected heatmap body
# -slice index of row slices in the heatmap
# -which on rows or on columns?
#
# == details
# There is a viewport for each dendrogram in the heatmap.
# This function contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
#
# If you know the number of leaves in the dendrogram, it is
# simple to calculate the position of every leave in the dendrogram.
# E.g., for the column dendrogram, the i^th leave is located at:
#
#     # assume nc is the number of columns 
#     unit((i-0.5)/nc, "npc")
#
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# set.seed(123)
# Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2)
# decorate_dend("mat", {
#     grid.rect(gp = gpar(fill = "#FF000080"))
# }, which = "row", slice = 2)
#
decorate_dend = function(heatmap, code, slice = 1, which = c("column", "row")) {
	
	which = match.arg(which)[1]
	if(which == "column") {
		vp_name = paste0(heatmap, "_dend_", which)
	} else if(which == "row") {
		if(is.null(slice)) {
			vp_name = paste0(heatmap, "_dend_", which, "_", 1)
			seekViewport(vp_name)
			upViewport()
		} else {
			vp_name = paste0(heatmap, "_dend_", which, "_", slice)
			seekViewport(vp_name)
		}
	}

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)

	seekViewport("global")
}

# == title
# Decorate heatmap dendrogram on columns
#
# == param
# -... pass to `decorate_dend`
#
# == details
# This is a wrapper function which pre-defined ``which`` argument in `decorate_dend`.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # No example for this function
# NULL
#
decorate_column_dend = function(...) {
	decorate_dend(..., which = "column")
}

# == title
# Decorate heatmap dendrogram on rows
#
# == param
# -... pass to `decorate_dend`
#
# == details
# This is a helper function which pre-defined ``which`` argument in `decorate_dend`.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # No example for this function
# NULL
#
decorate_row_dend = function(...) {
	decorate_dend(..., which = "row")
}


# == title
# Decorate the heatmap dimension names
#
# == param
# -heatmap name of the heatmap
# -code code that adds graphics in the selected heatmap body
# -slice index of row slices in the heatmap
# -which on rows or on columns?
#
# == details
# There is a viewport for row names and column names in the heatmap.
# This function contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
#
# If you know the dimensions of the matrix, it is
# simple to calculate the position of every row name or column name in the heatmap.
# E.g., for the column column, the i^th name is located at:
#
#     # assume nc is the number of columns 
#     unit((i-0.5)/nc, "npc")
#
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# set.seed(123)
# mat = matrix(rnorm(100), 10)
# rownames(mat) = letters[1:10]
# colnames(mat) = LETTERS[1:10]
# Heatmap(mat, name = "mat", km = 2)
# 
# decorate_dimnames("mat", {
#     grid.rect(gp = gpar(fill = "#FF000080"))
# }, which = "row", slice = 2)
#
decorate_dimnames = function(heatmap, code, slice = 1, which = c("column", "row")) {
	
	which = match.arg(which)[1]
	if(which == "column") {
		vp_name = paste0(heatmap, "_", which, "_names")
	} else if(which == "row") {
		if(is.null(slice)) {
			vp_name = paste0(heatmap, "_", which, "_names_", 1)
			seekViewport(vp_name)
			upViewport()
		} else {
			vp_name = paste0(heatmap, "_", which, "_names_", slice)
			seekViewport(vp_name)
		}
	}

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
	seekViewport("global")
}

# == title
# Decorate heatmap row names
#
# == param
# -... pass to `decorate_dimnames`
#
# == details
# This is a helper function which pre-defined ``which`` argument in `decorate_dimnames`.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # No example for this function
# NULL
#
decorate_row_names = function(...) {
	decorate_dimnames(..., which = "row")
}

# == title
# Decorate heatmap column names
#
# == param
# -... pass to `decorate_dimnames`
#
# == details
# This is a helper function which pre-defined ``which`` argument in `decorate_dimnames`.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # No example for this function
# NULL
#
decorate_column_names = function(...) {
	decorate_dimnames(..., which = "column")
}


# == title
# Decorate the heatmap title
#
# == param
# -heatmap name of the heatmap
# -code code that adds graphics in the selected heatmap body
# -slice index of row slices in the heatmap
# -which on rows or on columns?
#
# == details
# There is a viewport for row titles and column title in the heatmap.
# This function contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# set.seed(123)
# Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2)
# decorate_title("mat", {
#     grid.rect(gp = gpar(fill = "#FF000080"))
# }, which = "row", slice = 2)
#
decorate_title = function(heatmap, code, slice = 1, which = c("column", "row")) {
	
	which = match.arg(which)[1]
	if(which == "column") {
		vp_name = paste0(heatmap, "_", which, "_title")
	} else if(which == "row") {
		if(is.null(slice)) {
			vp_name = paste0(heatmap, "_", which, "_title_", 1)
			seekViewport(vp_name)
			upViewport()
		} else {
			vp_name = paste0(heatmap, "_", which, "_title_", slice)
			seekViewport(vp_name)
		}
	}

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
	seekViewport("global")
}

# == title
# Decorate heatmap row title
#
# == param
# -... pass to `decorate_title`
#
# == details
# This is a helper function which pre-defined ``which`` argument in `decorate_title`.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # No example for this function
# NULL
#
decorate_row_title = function(...) {
	decorate_title(..., which = "row")
}

# == title
# Decorate heatmap column title
#
# == param
# -... pass to `decorate_title`
#
# == details
# This is a helper function which pre-defined ``which`` argument in `decorate_title`.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # No example for this function
# NULL
#
decorate_column_title = function(...) {
	decorate_title(..., which = "column")
}

# == title
# Decorate the heatmap annotation
#
# == param
# -annotation name of the annotation
# -code code that adds graphics in the selected heatmap body
# -slice index of row slices in the heatmap
#
# == details
# There is a viewport for every column annotation and row annotation.
# This function contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
#
# == value
# The function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# set.seed(123)
# ha1 = HeatmapAnnotation(df = data.frame(type = rep(letters[1:2], 5)))
# ha2 = rowAnnotation(point = anno_points(runif(10), which = "row"))
# Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2,
#     top_annotation = ha1) + ha2
# decorate_annotation("type", {
#     grid.circle(x = unit(c(0.2, 0.4, 0.6, 0.8), "npc"), 
#         gp = gpar(fill = "#FF000080"))
# })
# decorate_annotation("point", {
#     grid.rect(gp = gpar(fill = "#FF000080"))
# }, slice = 2)
#
decorate_annotation = function(annotation, code, slice) {

	if(missing(slice)) {
		vp_name = paste0("annotation_", annotation)
		o = try(seekViewport(vp_name), silent = TRUE)
		if(inherits(o, "try-error")) {
			vp_name2 = paste0("annotation_", annotation, "_", 1)
			o = try(seekViewport(vp_name2), silent = TRUE)
			if(inherits(o, "try-error")) {
				stop(paste0("Cannot find viewport ", vp_name, " or ", vp_name2, "\n"))
			}
			vp_name = vp_name2
		}
		seekViewport(vp_name)
	} else {
		if(is.null(slice)) {
			vp_name = paste0("annotation_", annotation, "_", 1)
			seekViewport(vp_name)
			upViewport()
		} else {
			vp_name = paste0("annotation_", annotation, "_", slice)
			seekViewport(vp_name)
		}
	}

	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
	seekViewport("global")
}

