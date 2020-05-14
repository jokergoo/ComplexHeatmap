# == title
# Select an area in the heatmap
#
# == param
# -ht_list A `HeatmapList-class` object returned by `draw,Heatmap-method` or `draw,HeatmapList-method`.
# -mark Whether mark the selected area as a rectangle.
# -pos1 If the value is ``NULL``, it can be selected by click on the heatmap (of cource, the heatmap should be on
#       the interactive graphic device). If it is set, it must be a `grid::unit` object with length two which
#       corresponds to the x and y position of the point.
# -pos2 Another point as ``pos1``, together with ``pos1`` defines the selected region.
# -verbose Whether to print messages.
#
# == details
# The regions can be selected interactively or manually by setting ``pos1`` and ``pos2``.
#
# == value
# A deep list containing row indices and column indices corresponding to the selected region.
# The format is (assme the returned object is called ``lt``):
#
# - ``lt[[ heatmap_name ]][[ slice_name ]][[ row_index ]]``: row indicies of the selected sub-matrix.
# - ``lt[[ heatmap_name ]][[ slice_name ]][[ column_index ]]``: column indicies of the selected sub-matrix.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == seealso
# With this function, it is possible to make ComplexHeatmap interactive as a shiny app. Try `selectArea_shiny_demo`.
#
# == example
# m = matrix(rnorm(100), 10)
# rownames(m) = 1:10
# colnames(m) = 1:10
#
# ht = Heatmap(m)
# ht = draw(ht)
# selectArea(ht, pos1 = unit(c(1, 1), "cm"), pos2 = unit(c(4, 4), "cm"))
# # you can try selectArea(ht) in the interactive graphic device
#
# set.seed(123)
# ht = Heatmap(m, row_km = 2, column_km = 2)
# ht = draw(ht)
# selectArea(ht, pos1 = unit(c(1, 1), "cm"), pos2 = unit(c(8, 8), "cm"))
# # you can try selectArea(ht) in the interactive graphic device
#
selectArea = function(ht_list, pos1 = NULL, pos2 = NULL, mark = TRUE, verbose = TRUE) {

	if(is.null(pos1) || is.null(pos2)) pos2 = pos1 = NULL
	if(!is.null(pos1)) {
		if(!inherits(pos1, "simpleUnit")) {
			stop_wrap("`pos1` should be a simpleUnit.")
		}
		if(!inherits(pos2, "simpleUnit")) {
			stop_wrap("`pos2` should be a simpleUnit.")
		}

		if(length(pos1) != 2) {
			stop_wrap("Length of `pos1` should be 2 (x and y).")
		}

		if(length(pos2) != 2) {
			stop_wrap("Length of `pos2` should be 2 (x and y).")
		}

		if(!identical(unitType(pos1), unitType(pos2))) {
			stop_wrap("`pos1` should have the same unit as `pos2`.")
		}

		pos1 = list(x = pos1[1], y = pos1[2])
		pos2 = list(x = pos2[1], y = pos2[2])
	}

	seekViewport("global")
	
	if(is.null(pos1)) {
		if(!dev.interactive()) {
			stop_wrap("Graphic device should be interactive if you want to manually select the regions.")
		}

		if(verbose) cat("Click two positions on the heatmap (double click or right click on\nthe plot to cancel):\n")

		unit = "mm"
		pos1 = grid.locator(unit = unit)
		if(is.null(pos1)) {
			if(verbose) cat("Canceled.\n")
			return(invisible(NULL))
		}
	} else {
		unit = unitType(pos1$x)
	}
	pos1 = lapply(pos1, as.numeric)
	if(mark) grid.points(pos1$x, pos1$y, default.units = unit)
	if(verbose) qqcat("  Point1: x = @{sprintf('%.1f', pos1$x)} @{unit}, y = @{sprintf('%.1f', pos1$y)} @{unit} (measured in the graphic device)\n")
	
	if(is.null(pos2)) {
		unit = "mm"
		pos2 = grid.locator(unit = unit)
		if(is.null(pos2)) {
			if(verbose) cat("Canceled.\n")
			return(invisible(NULL))
		}
	} else {
		unit = unitType(pos2$x)
	}
	pos2 = lapply(pos2, as.numeric)
	if(mark) grid.points(pos2$x, pos2$y, default.units = unit)
	if(verbose) qqcat("  Point2: x = @{sprintf('%.1f', pos2$x)} @{unit}, y = @{sprintf('%.1f', pos2$y)} @{unit} (measured in the graphic device)\n")
	
	if(verbose) cat("\n")

	if(mark) {
		grid.rect( (0.5*pos1$x + 0.5*pos2$x), (0.5*pos1$y + 0.5*pos2$y),
			       pos2$x - pos1$x, pos2$y - pos1$y,
			       default.units = unit, gp = gpar(fill = NA))
	}

	#### pos1 should always be on the bottom left and pos2 on the top right
	if(pos1$x > pos2$x) {
		tmp = pos1$x
		pos1$x = pos2$x
		pos2$x = tmp
	}
	if(pos1$y > pos2$y) {
		tmp = pos1$y
		pos1$y = pos2$y
		pos2$y = tmp
	}

	ht_pos = ht_pos_on_device(ht_list, unit, valueOnly = TRUE)

	res = list()
	for(ht_name in names(ht_pos)) {

		ht = ht_list@ht_list[[ht_name]]
		res[[ht_name]] = list()

		if(verbose) qqcat("Search in heatmap '@{ht_name}'\n")

		for(vp_name in names(ht_pos[[ht_name]])) {
			i = as.numeric(gsub(".*_(\\d+)_\\d+$", "\\1", vp_name))
			j = as.numeric(gsub(".*_(\\d+)$", "\\1", vp_name))

			if(verbose) qqcat("  - row slice @{i}, column slice @{j} [@{vp_name}]... ")
			
			vp_min_x = ht_pos[[ht_name]][[vp_name]]$x[1]	
			vp_max_x = ht_pos[[ht_name]][[vp_name]]$x[2]
			vp_min_y = ht_pos[[ht_name]][[vp_name]]$y[1]	
			vp_max_y = ht_pos[[ht_name]][[vp_name]]$y[2]

			nc = length(ht@column_order_list[[j]])
			ind1 = ceiling((pos1$x - vp_min_x) / (vp_max_x - vp_min_x) * nc)
			ind2 = ceiling((pos2$x - vp_min_x) / (vp_max_x - vp_min_x) * nc)
			if(ind1 <= 0 && ind2 <= 0) { # the region is on the left of the heatmap
				res[[ht_name]][[vp_name]]$column_index = integer(0)
			} else if(ind1 > nc && ind2 > nc) { # the region in on the right of the heatmap
				res[[ht_name]][[vp_name]]$column_index = integer(0)
			} else {
				if(ind1 <= 0) ind1 = 1
				if(ind2 >= nc) ind2 = nc

				if(ind1 < ind2) {
					res[[ht_name]][[vp_name]]$column_index = ht@column_order_list[[j]][ind1:ind2]
				} else {
					res[[ht_name]][[vp_name]]$column_index = ht@column_order_list[[j]][ind2:ind1]
				}
			}

			nr = length(ht@row_order_list[[i]])
			ind1 = 1 + nr - ceiling((pos1$y - vp_min_y) / (vp_max_y - vp_min_y) * nr)
			ind2 = 1 + nr - ceiling((pos2$y - vp_min_y) / (vp_max_y - vp_min_y) * nr)
			if(ind1 <= 0 && ind2 <= 0) { # the region is on the bottom of the heatmap
				res[[ht_name]][[vp_name]]$row_index = integer(0)
			} else if(ind1 > nr && ind2 > nr) { # the region in on the top of the heatmap
				res[[ht_name]][[vp_name]]$row_index = integer(0)
			} else {
				if(ind2 <= 0) ind2 = 1
				if(ind1 >= nr) ind1 = nr
				if(ind1 < ind2) {
					res[[ht_name]][[vp_name]]$row_index = ht@row_order_list[[i]][ind1:ind2]
				} else {
					res[[ht_name]][[vp_name]]$row_index = ht@row_order_list[[i]][ind2:ind1]
				}
			}

			if(length(res[[ht_name]][[vp_name]]$column_index) == 0) res[[ht_name]][[vp_name]]$row_index = integer(0)
			if(length(res[[ht_name]][[vp_name]]$row_index) == 0) res[[ht_name]][[vp_name]]$column_index = integer(0)
			
			if(length(res[[ht_name]][[vp_name]]$column_index) == 0) {
				if(verbose) cat("not overlap\n")
			} else {
				if(verbose) cat("overlap\n")
			}
		}
	}
	if(verbose) cat("\n")
	res
}


# == title
# Get the heatmap positions on the graphic device
#
# == param
# -ht_list A `HeatmapList-class` object returned by `draw,Heatmap-method` or `draw,HeatmapList-method`.
# -unit The unit.
# -valueOnly Whether only return the numeric values.
#
# == value
# It returns a deep list of the positions of the bottom left and top right
# of every heatmap slice.
#
# The format is (assme the returned object is called ``pos``):
#
# - ``pos[[ heatmap_name ]][[ slice_name ]][[ x ]]``: left and right of the selected sub-heatmap.
# - ``pos[[ heatmap_name ]][[ slice_name ]][[ y ]]``: bottom and top of the selected sub-heatmap.
#
# == example
# \dontrun{
# # it is runable if you are under an interactive graphic device
# m = matrix(rnorm(100), 10)
# ht = Heatmap(m, row_km = 2, column_km = 2)
# ht = draw(ht)
# pos = ht_pos_on_device(ht)
#
# ds = dev.size()
# dev.new(width = ds[1], height = ds[2])
# grid.newpage()
# for(ht_name in names(pos)) {
# 	for(vp_name in names(pos[[ht_name]])) {
# 		x = pos[[ht_name]][[vp_name]]$x
# 		y = pos[[ht_name]][[vp_name]]$y
# 		pushViewport(viewport(x = x[1], y = y[1], 
# 			width = x[2] - x[1], height = y[2] - y[1],
# 			just = c("left", "bottom")))
# 		grid.rect()
# 		popViewport()
# 	}
# }
# }
ht_pos_on_device = function(ht_list, unit = "inch", valueOnly = FALSE) {
	
	oe = try(seekViewport("global"), silent = TRUE)
	if(inherits(oe, "try-error")) {
		stop_wrap("No heatmap is on the graphic device.")
	}

	if(inherits(ht_list, "Heatmap")) {
		stop_wrap("`ht_list` should be returned by `draw()`.")
	}

	if(!ht_list@layout$initialized) {
		stop_wrap("`ht_list` should be returned by `draw()`.")
	}

	all_ht_names = names(ht_list@ht_list)
	all_ht_names = all_ht_names[sapply(ht_list@ht_list, inherits, "Heatmap")]
	l = duplicated(all_ht_names)
	if(any(l)) {
		stop_wrap("Heatmap names should not be duplicated.")
	}

	lt = list()
	for(i in seq_along(ht_list@ht_list)) {
		if(inherits(ht_list@ht_list[[i]], "Heatmap")) {
			ht = ht_list@ht_list[[i]]
			ht_name = ht@name

			lt[[ht_name]] = list()

			for(i in seq_along(ht@row_order_list)) {
				for(j in seq_along(ht@column_order_list)) {

					vp_name = qq("@{ht_name}_heatmap_body_@{i}_@{j}")

					seekViewport(vp_name)
					loc = deviceLoc(x = unit(0, "npc"), y = unit(0, "npc"))
					vp_min_x = convertX(loc[[1]], unit)
					vp_min_y = convertY(loc[[2]], unit)

					loc = deviceLoc(x = unit(1, "npc"), y = unit(1, "npc"))
					vp_max_x = convertX(loc[[1]], unit)
					vp_max_y = convertY(loc[[2]], unit)
					
					if(valueOnly) {
						lt[[ht_name]][[vp_name]] = list(x = c(as.numeric(vp_min_x), as.numeric(vp_max_x)),
							                            y = c(as.numeric(vp_min_y), as.numeric(vp_max_y)))
					} else {
						lt[[ht_name]][[vp_name]] = list(x = unit.c(vp_min_x, vp_max_x),
							                            y = unit.c(vp_min_y, vp_max_y))
					}
					
				}
			}
		}
	}

	lt
}

seek_root_vp = function() {
	seekViewport(grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name[2])
	upViewport(1)
}

# == title
# A demo of using selectArea() as a shiny app
#
# == param
# -ht_list A `Heatmap-class` or a `HeatmapList-class` object.
#
# == details
# source code of the app is at ``system.file("app", package = "ComplexHeatmap")``.
#
# == example
# if(interactive()) {
#     selectArea_shiny_demo()
# }
#
# # by providing a heatmap/heatmap list
# if(interactive()) {
#     m = matrix(rnorm(100), 10)
#     rownames(m) = 1:10
#     colnames(m) = 1:10
#
#     ht = Heatmap(m)
#     selectArea_shiny_demo(ht)
# }
selectArea_shiny_demo = function(ht_list) {
	if(!requireNamespace("shiny")) {
		stop_wrap("shiny package should be installed.")
	}
	if(missing(ht_list)) {
		cat("No heatmap is provides, use a random matrix.\n")
		m = matrix(rnorm(100), 10)
		rownames(m) = 1:10
		colnames(m) = 1:10

		ht_list = Heatmap(m)
	} else {
		ht_list = ht_list
	}
	source(system.file("app", "app.R", package = "ComplexHeatmap"), local = TRUE)
	shiny::shinyApp(get("ui"), get("server"))
}
