# == title
# Decorate the heatmap body
#
# == param
# -heatmap name of the heatmap
# -code code that is executed in the heatmap body
# -slice index of row slices in the heatmap
#
# == details
# This simple function actually contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
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
decorate_heatmap_body = function(heatmap, code = {}, slice = 1) {

	vp_name = paste0(heatmap, "_heatmap_body_", slice)

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
}

# == title
# Decorate the heatmap dendrogram
#
# == param
# -heatmap name of the heatmap
# -code code that is executed in the heatmap body
# -slice index of row slices in the heatmap
# -which on rows or on columns?
#
# == details
# This simple function actually contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# set.seed(123)
# Heatmap(matrix(rnorm(100), 10), name = "mat", km = 2)
# decorate_hclust("mat", {
#     grid.rect(gp = gpar(fill = "#FF000080"))
# }, which = "row", slice = 2)
#
decorate_hclust = function(heatmap, code, slice = 1, which = c("column", "row")) {
	
	which = match.arg(which)[1]
	if(which == "column") {
		vp_name = paste0(heatmap, "_hclust_", which)
	} else if(which == "row") {
		vp_name = paste0(heatmap, "_hclust_", which, "_", slice)
	}

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
}

# == title
# Decorate heatmap dendrogram on columns
#
# == param
# -... pass to `decorate_hclust`
#
# == details
# This is a wrapper function which pre-defined ``which`` argument in `decorate_hclust`.
#
decorate_column_hclust = function(...) {
	decorate_hclust(..., which = "column")
}

# == title
# Decorate heatmap dendrogram on rows
#
# == param
# -... pass to `decorate_hclust`
#
# == details
# This is a wrapper function which pre-defined ``which`` argument in `decorate_hclust`.
#
decorate_row_hclust = function(...) {
	decorate_hclust(..., which = "row")
}


# == title
# Decorate the heatmap dimension names
#
# == param
# -heatmap name of the heatmap
# -code code that is executed in the heatmap body
# -slice index of row slices in the heatmap
# -which on rows or on columns?
#
# == details
# This simple function actually contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
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
		vp_name = paste0(heatmap, "_", which, "_names_", slice)
	}

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
}

# == title
# Decorate heatmap row names
#
# == param
# -... pass to `decorate_dimnames`
#
# == details
# This is a wrapper function which pre-defined ``which`` argument in `decorate_dimnames`.
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
# This is a wrapper function which pre-defined ``which`` argument in `decorate_dimnames`.
#
decorate_column_names = function(...) {
	decorate_dimnames(..., which = "column")
}


# == title
# Decorate the heatmap title
#
# == param
# -heatmap name of the heatmap
# -code code that is executed in the heatmap body
# -slice index of row slices in the heatmap
# -which on rows or on columns?
#
# == details
# This simple function actually contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
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
		vp_name = paste0(heatmap, "_", which, "_title_", slice)
	}

	seekViewport(vp_name)
	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
}

# == title
# Decorate heatmap row title
#
# == param
# -... pass to `decorate_title`
#
# == details
# This is a wrapper function which pre-defined ``which`` argument in `decorate_title`.
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
# This is a wrapper function which pre-defined ``which`` argument in `decorate_title`.
#
decorate_column_title = function(...) {
	decorate_title(..., which = "column")
}

# == title
# Decorate the heatmap annotation
#
# == param
# -annotation name of the annotation
# -code code that is executed in the heatmap body
# -slice index of row slices in the heatmap
#
# == details
# This simple function actually contructs the name of the viewport,
# goes to the viewport by `grid::seekViewport` and applies code
# to that viewport.
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
decorate_annotation = function(annotation, code, slice = NULL) {

	if(is.null(slice)) {
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
		vp_name = paste0("annotation_", annotation, "_", slice)
		seekViewport(vp_name)
	}

	e = new.env(parent = parent.frame())
	eval(substitute(code), envir = e)
}

