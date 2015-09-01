
# == title
# Select a region in the heatmap
#
# == param
# -mark whether show the rectangular selected region
#
# == details
# Users can use mouse to click two positions on the heatmap, the function
# will return the row index and column index for the selected region in the selected matrix.
# 
select_region = function(mark = FALSE) {

	if(!interactive()) {
		stop("`select()` can only be used under interactive mode.")
	}

	x = dev.cur()
	if(! (names(x) %in% c("quartz", "X11", "windows", "JavaGD", "CairoX11", "CairoWin")) ) {
		stop("Can not detect any interactive graphic device.")
	}

	seekViewport("main_heatmap_list")

	cat("Click two positions on the heatmap:\n")
	pos1 = grid.locator(unit = "mm")
	cat("  x:", sprintf("%.1f", pos1$x), "mm, y:", sprintf("%.1f", pos1$y), "mm\n")
	pos2 = grid.locator(unit = "mm")
	cat("  x:", sprintf("%.1f", pos2$x), "mm, y:", sprintf("%.1f", pos2$y), "mm\n")

	# pos1 is always at bottom left
	# pos2 is always at top right
	if(compare_width(pos1$x, pos2$x) > 0) {
		tmp = pos2$x
		pos2$x = pos1$x
		pos1$x = tmp
	}

	if(compare_height(pos1$y, pos2$y) > 0) {
		tmp = pos2$y
		pos2$y = pos1$y
		pos1$y = tmp
	}

	for(i in seq_along(.LAST_HT_LIST$object@ht_list)) {
		if(inherits(.LAST_HT_LIST$object@ht_list[[i]], "Heatmap")) {
			ht = .LAST_HT_LIST$object@ht_list[[i]]
			ht_name = ht@name
			
			seekViewport(qq("heatmap_@{ht_name}", code.pattern = "@\\{CODE\\}"))
			vp = current.viewport()

			pos1_cp = list()
			pos2_cp = list()

			# relative to current heatmap body
			pos1_cp$x = pos1$x - convertWidth(vp$x, "mm") - sum(component_width(ht, 1:3))
			pos1_cp$y = pos1$y - convertHeight(vp$y, "mm") - sum(component_height(ht, 6:9))
			pos2_cp$x = pos2$x - convertWidth(vp$x, "mm") - sum(component_width(ht, 1:3))
			pos2_cp$y = pos2$y - convertHeight(vp$y, "mm") - sum(component_height(ht, 6:9))

			pos1_cp$x = convertWidth(pos1_cp$x, "mm")
			pos1_cp$y = convertHeight(pos1_cp$y, "mm")
			pos2_cp$x = convertWidth(pos2_cp$x, "mm")
			pos2_cp$y = convertHeight(pos2_cp$y, "mm")

			for(i in seq_along(ht@row_order_list)) {
				
				pos1_cp2 = list()
				pos2_cp2 = list()

				seekViewport(qq("@{ht_name}_heatmap_body_@{i}"))
				vp2 = current.viewport()
				pos1_cp2$x = pos1_cp$x
				pos1_cp2$y = pos1_cp$y - (vp2$y - vp2$height)
				pos2_cp2$x = pos2_cp$x
				pos2_cp2$y = pos2_cp$y - (vp2$y - vp2$height)

				pos1_cp2$x = convertWidth(pos1_cp2$x, "mm")
				pos1_cp2$y = convertHeight(pos1_cp2$y, "mm")
				pos2_cp2$x = convertWidth(pos2_cp2$x, "mm")
				pos2_cp2$y = convertHeight(pos2_cp2$y, "mm")
			
				# test whether two clicks are in one heatmap body
				if(compare_width(pos1_cp2$x) < 0 || compare_height(pos1_cp2$y) < 0 ||
				   compare_width(pos2_cp2$x) < 0 || compare_height(pos2_cp2$y) < 0 ||
				   compare_width(pos1_cp2$x, vp2$width) > 0 || compare_width(pos2_cp2$x, vp2$width) > 0 ||
				   compare_height(pos1_cp2$y, vp2$height) > 0 || compare_height(pos2_cp2$y, vp2$height) > 0) {
					
				} else {
					# extract row index and column index
					# be careful with row slices
					ht_width = convertWidth(vp2$width, "mm")
					ht_height = convertHeight(vp2$height, "mm")

					res = list()

					nc = length(ht@column_order)

					x1 = ceiling(as.numeric(pos1_cp2$x) / as.numeric(ht_width) * nc)
					x2 = ceiling(as.numeric(pos2_cp2$x) / as.numeric(ht_width) * nc)

					res$column_order = ht@column_order[x1:x2]

					nr = length(ht@row_order_list[[i]])

					y1 = 1 +nr - ceiling(as.numeric(pos1_cp2$y) / as.numeric(ht_height) * nr)
					y2 = 1 + nr - ceiling(as.numeric(pos2_cp2$y) / as.numeric(ht_height) * nr)

					res$row_order = ht@row_order_list[[i]][y2:y1]

					if(mark) {
						grid.rect( (pos1_cp2$x + pos2_cp2$x)*0.5, (pos1_cp2$y + pos2_cp2$y)*0.5,
							       abs_unit(pos2_cp2$x - pos1_cp2$x), abs_unit(pos2_cp2$y - pos1_cp2$y) )
					}

					return(res)
				}
			}
		} else {
			stop("Do not click into row annotation regions.\n")
		}
	}

	cat("\nTwo clicks should be in one same heatmap (or slice) region.\n\n")
	select_region()

}

compare_width = function(u1, u2 = unit(0, "mm")) {

	u1 = convertWidth(u1, "mm", valueOnly = TRUE)
	u2 = convertWidth(u2, "mm", valueOnly = TRUE)

	ifelse(u1 > u2, 1, ifelse(u1 < u2, -1, 0))
}


compare_height = function(u1, u2 = unit(0, "mm")) {

	u1 = convertHeight(u1, "mm", valueOnly = TRUE)
	u2 = convertHeight(u2, "mm", valueOnly = TRUE)

	ifelse(u1 > u2, 1, ifelse(u1 < u2, -1, 0))
}

abs_unit = function(u) {
	if(compare_unit(u) < 0) u = -1*u
	return(u)
}


