
# == title
# Draw 3D bars
#
# == param
# -x x coordinate of the center point in the bottom face.
# -y y coordinate of the center point in the bottom face.
# -w Width of the botton face.
# -h Height of the botton face.
# -l Length of the bars (in the z-direction).
# -theta The angle for the projection.
# -default.units Units.
# -fill Filled colors for the bars.
# -col Border colors.
#
# == example
# grid.newpage()
# bar3D(c(0.3, 0.7), 0.5, 0.2, 0.2, 0.2, fill = 2:3)
#
bar3D = function(x, y, w, h, l, theta = 60, default.units = "npc", fill = "white", col = "black") {

	if(!is.unit(x)) x = unit(x, default.units)
	if(!is.unit(y)) y = unit(y, default.units)
	if(!is.unit(w)) w = unit(w, default.units)
	if(!is.unit(h)) h = unit(h, default.units)
	if(!is.unit(l)) l = unit(l, default.units)

	nx = length(x)
	ny = length(y)
	nw = length(w)
	nh = length(h)
	nl = length(l)

	n = max(nx, ny, nw, nh, nl)
	if(nx == 1) x = rep(x, n)
	if(ny == 1) y = rep(y, n)
	if(nw == 1) w = rep(w, n)
	if(nh == 1) h = rep(h, n)
	if(nl == 1) l = rep(l, n)
	if(length(fill) == 1) fill = rep(fill, n)

	if(any(theta < 0 | theta > 90)) {
		stop_wrap("`theta` can only take value between 0 and 90.")
	}

	x1 = x - w*0.5
	x2 = x + w*0.5
	y1 = y - h*0.5
	y2 = y + h*0.5

	xu = x + l * cos(theta/180*pi)
	yu = y + l * sin(theta/180*pi)

	a1 = xu - w*0.5
	a2 = xu + w*0.5
	b1 = yu - h*0.5
	b2 = yu + h*0.5

	id = 1
	px = NULL
	py = NULL
	pid = NULL
	pfill = NULL

	for(i in 1:n) {

		fill2 = add_luminance(fill[i])

		# front
		if(is.null(px)) {
			px = unit.c(x1[i], a1[i], a2[i], x2[i])
		} else {
			px = unit.c(px, x1[i], a1[i], a2[i], x2[i])
		}
		if(is.null(py)) {
			py = unit.c(y1[i], b1[i], b1[i], y1[i])
		} else {
			py = unit.c(py, y1[i], b1[i], b1[i], y1[i])
		}
		pid = c(pid, rep(id, 4))
		pfill = c(pfill, fill2[2])
		id = id + 1

		# left
		px = unit.c(px, x1[i], x1[i], a1[i], a1[i])
		py = unit.c(py, y1[i], y2[i], b2[i], b1[i])
		pid = c(pid, rep(id, 4))
		pfill = c(pfill, fill2[3])
		id = id + 1

		# top
		px = unit.c(px, a1[i], a1[i], a2[i], a2[i])
		py = unit.c(py, b1[i], b2[i], b2[i], b1[i])
		pid = c(pid, rep(id, 4))
		pfill = c(pfill, fill2[1])
		id = id + 1
	}

	grid.polygon(px, py, id = pid, gp = gpar(fill = pfill, col = col))
}

# for a single color
add_luminance = function(col) {
	rgb = col2rgb(col)
	r = rgb[1, ]/255
	g = rgb[2, ]/255
	b = rgb[3, ]/255
	col = as(RGB(r, g, b), "polarLUV")
	value = coords(col)

	sequential_hcl(
		n = 9,
		h = value[, "H"],
		c = value[, "C"],
		l = value[1, "L"]
	)
}


# == title
# 3D Heatmap
#
# == param
# -matrix The input matrix. Values should be non-negative.
# -... All pass to `Heatmap`.
# -bar_rel_width A factor between 0 and 1.
# -bar_rel_height A factor between 0 and 1.
# -bar_max_length Maximal length of bars. Value should be in absolute unit.
# -bar_angle Angle for the projection.
# -row_names_side Row names are by default put on the left side of the heatmap.
# -show_row_dend By default the dendrogram is not drawn.
# -show_column_dend By default the dendrogram is not drawn.
#
# == detals
# For large matrices, the plotting might be slow.
#
# == example
# m = matrix(sample(100, 36), 6)
# Heatmap3D(m) 
Heatmap3D = function(matrix, 
	..., 

	bar_rel_width = 0.6,
	bar_rel_height = 0.6,
	bar_max_length = unit(1, "cm"),
	bar_angle = 60,
	
	row_names_side = "left",
	show_row_dend = FALSE,
	show_column_dend = FALSE) {

	if(any(matrix < 0)) {
		stop_wrap("The matrix should be non-negative.")
	}

	if(bar_rel_width < 0 | bar_rel_width > 1) {
		stop_wrap("`bar_rel_width` can only be set to a value between 0 and 1.")
	}
	if(bar_rel_height < 0 | bar_rel_height > 1) {
		stop_wrap("`bar_rel_height` can only be set to a value between 0 and 1.")
	}

	ht = Heatmap(matrix, ..., rect_gp = gpar(type = "none"),
		layer_fun = function(j, i, x, y, w, h, f) {
			v = pindex(matrix, i, j)
			od = rank(order(-as.numeric(y), -as.numeric(x)))
			grid.rect(x[od], y[od], w[od], h[od], gp = gpar(col = "white", fill = "#EEEEEE"))
			bar3D(x[od], y[od], w[od]*bar_rel_width, h[od]*bar_rel_height, v[od]/max(matrix)*bar_max_length, theta = bar_angle, fill = f[od])	
		},
		show_row_dend = show_row_dend,
		show_column_dend = show_column_dend,
		row_names_side = row_names_side,
	)
	ht@heatmap_param$type = "Heatmap3D"
	ht
}
