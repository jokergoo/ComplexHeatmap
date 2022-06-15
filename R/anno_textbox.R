
# == title
# A simple grob for the word cloud
#
# == param
# -text A vector of texts. The value can be single words or phrases/sentenses.
# -x X position.
# -y Y position.
# -just Justification of the box in the viewport.
# -gp Graphics parameters of texts.
# -background_gp Graphics parameters for the box.
# -round_corners Whether to draw round corners for the box.
# -r Radius of the round corners.
# -line_space Space between lines. The value can be a `grid::unit` object or a numeric scalar which is measured in mm.
# -text_space Space between texts The value can be a `grid::unit` object or a numeric scalar which is measured in mm.
# -max_width The maximal width of the viewport to put the word cloud. The value can be a `grid::unit` object or a numeric scalar which is measured in mm.
#        Note this might be larger than the final width of the returned grob object.
# -padding Padding of the box, i.e. space between text and the four box borders. The value should be a `grid::unit` object with length 1, 2 or 4. If 
#    length of the input unit is 2, the first value is the padding both to the top and to the bottom, and the second value is the padding to the left and right.
#    If length of the input unit is 4, the four values correspond to paddings to the bottom, left, top and right of the box.
# -first_text_from Should the texts be added from the top of the box or from the bottom? Value should be either "top" or "bottom".
# -add_new_line Whether to add new line after every text? If ``TRUE``, each text will be in a separated line.
# -word_wrap Whether to apply word wrap for phrases/sentenses.
#
# == value
# A `grid::grob` object. The width and height of the grob can be get by `grid::grobWidth` and `grid::grobHeight`.
#
# == example
# words = sapply(1:30, function(x) strrep(sample(letters, 1), sample(3:10, 1)))
# grid.newpage()
# grid.textbox(words, gp = gpar(fontsize = runif(30, min = 5, max = 30)))
#
# sentenses = c("This is sentense 1", "This is a long long long long long long long sentense.")
# grid.newpage()
# grid.textbox(sentenses)
# grid.textbox(sentenses, word_wrap = TRUE)
# grid.textbox(sentenses, word_wrap = TRUE, add_new_line = TRUE)
#
textbox_grob = function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"), just = "centre", 
	gp = gpar(), background_gp = gpar(col = "black", fill = "transparent"), round_corners = FALSE, r = unit(0.1, "snpc"),
	line_space = unit(4, "pt"), text_space = unit(4, "pt"), max_width = unit(100, "mm"), 
	padding = unit(4, "pt"), first_text_from = "top", add_new_line = FALSE, word_wrap = FALSE) { # width in mm

	n_text = length(text)
	if(is.null(gp$col)) {
		if(package_version(packageDescription("circlize", fields = "Version")) < "0.4.14") {
			stop_wrap("Random color generation needs circlize package >= 0.4.14. Please upgrade it.")
		}
		gp$col = rand_color(n_text, friendly = TRUE)
	}
	for(nm in c("fontsize", "fontfamily", "fontface")) {
		if(is.null(gp[[nm]])) {
			gp[[nm]] = rep(get.gpar(nm)[[1]], n_text)
		}
	}

	gp = recycle_gp(gp, n_text)

	vp_x = x
	vp_y = y
	vp_just = just

	text_lt = list(text = text, space_after = rep(TRUE, n_text), new_line_after = rep(FALSE, n_text))
	if(add_new_line && !word_wrap) {
		text_lt$new_line_after = rep(TRUE, n_text)
	} else if(word_wrap) {

		if(first_text_from == "bottom") {
			od = rev(seq_len(n_text))
			text = text[od]
			gp = subset_gp(gp, od)
			first_text_from = "top"
		}

		words = strsplit(text, "\\s+")
		words = lapply(words, function(x) {
			n = length(x)
			x2 = rep(" ", 2*n - 1)
			x2[2*(1:n) - 1] = x
			x2
		})
		nw = sapply(words, length)

		gp2 = gpar()
		gp2$col = rep(gp$col, times = nw)
		gp2$fontsize = rep(gp$fontsize, times = nw)
		gp2$fontfamily = rep(gp$fontfamily, times = nw)
		gp2$fontface = rep(gp$fontface, times = nw)

		text_lt = list(text = unlist(words), space_after = rep(FALSE, sum(nw)), new_line_after = rep(FALSE, sum(nw)))
		text_lt$space_after[cumsum(nw)] = TRUE
		if(add_new_line) {
			text_lt$new_line_after[cumsum(nw)] = TRUE
		}
		gp = gp2
	}

	if(length(dev.list()) == 0) {
		dev.new()
	}

	n = length(text_lt$text)
	text_gb_lt = lapply(seq_len(n), function(i) textGrob(text_lt$text[i], gp = subset_gp(gp, i)))
	text_width = vapply(text_gb_lt, function(gb) convertWidth(grobWidth(gb), "mm", valueOnly = TRUE), 0)
	text_height = vapply(text_gb_lt, function(gb) convertHeight(grobHeight(gb), "mm", valueOnly = TRUE), 0)

	if(is.unit(line_space)) line_space = convertHeight(line_space, "mm", valueOnly = TRUE)
	if(is.unit(text_space)) text_space = convertWidth(text_space, "mm", valueOnly = TRUE)

	x = numeric(n)
	y = numeric(n)

	if(is.unit(max_width)) {
		max_width = convertWidth(max_width, "mm", valueOnly = TRUE)
	} 

	w = max(text_width)
	max_width = max(max_width, max(text_width))

	if(first_text_from == "bottom") {
		# the first text
		current_line_width = text_width[1]
		x[1] = 0
		y[1] = 0

		h = text_height[1]

		for(i in seq_len(n)[-1]) {
			# the next text can be put on the same line
			if(current_line_width + text_width[i] + text_space > max_width || text_lt$new_line_after[i-1]) {
				x[i] = 0
				y[i] = h + line_space
				current_line_width = text_width[i]
				w = max(w, current_line_width)
				h = y[i] + text_height[i]

			} else {
				x[i] = current_line_width + text_space*text_lt$space_after[i-1]
				y[i] = y[i-1] # same as previous one
				current_line_width = x[i] + text_width[i]
				w = max(w, current_line_width)
				h = max(h, y[i] + text_height[i])
			}
		}
		just = c(0, 0)
	} else if(first_text_from == "top") {
		current_line_width = text_width[1]
		x[1] = 0
		y[1] = 0

		h = -text_height[1]

		prev_line_ind = 1

		for(i in seq_len(n)[-1]) {
			if(current_line_width + text_width[i] + text_space > max_width || text_lt$new_line_after[i-1]) {
				y[prev_line_ind] = h

				prev_line_ind = i
				x[i] = 0
				y[i] = h - line_space
				current_line_width = text_width[i]
				w = max(w, current_line_width)
				h = y[i] - text_height[i]
			} else {
				x[i] = current_line_width + text_space*text_lt$space_after[i-1]
				y[i] = y[i-1] # same as previous one
				current_line_width = x[i] + text_width[i]
				w = max(w, current_line_width)
				h = min(h, y[i] - text_height[i])

				prev_line_ind = c(prev_line_ind, i)
			}
		}
		y[prev_line_ind] = h
		y = y - h
		h = -h

		just = c(0, 0)
	} else {
		stop("`first_text_from` can be 'top' or 'bottom'")
	}

	if(length(padding) == 1) {
		padding = rep(padding, 4)
	} else if(length(padding) == 2) {
		padding = unit.c(padding[1], padding[2], padding[1], padding[2])
	}
	padding = convertWidth(padding, "mm", valueOnly = TRUE)

	w = w + padding[2] + padding[4]
	h = h + padding[1] + padding[3]
	x = x + padding[2]
	y = y + padding[1]

	gl = gList(
		if(round_corners) {
			roundrectGrob(gp = background_gp, r = r)
		} else {
			rectGrob(gp = background_gp)
		},
		textGrob(text_lt$text, x = x, y = y, gp = gp, default.units = "mm", just = just)
		# rectGrob(x = x, y = y, width = text_width, height = text_height, default.units = "mm", just = c(0, 0))
	)

	gb = gTree(children = gl, cl = "textbox", 
		vp = viewport(x = vp_x, y = vp_y, just = vp_just, width = unit(w, "mm"), 
			          height = unit(h, "mm")))
	return(gb)
}

# == title
# Width for textbox grob
#
# == param
# -x The ``textbox`` grob returned by `textbox_grob`.
#
# == value
# A `grid::unit` object.
widthDetails.textbox = function(x) {
	x$vp$width
}

# == title
# Height for textbox grob
#
# == param
# -x The ``textbox`` grob returned by `textbox_grob`.
#
# == value
# A `grid::unit` object.
heightDetails.textbox = function(x) {
	x$vp$height
}

# == title
# Draw multiple texts in a box
#
# == param
# -text A vector of texts. The value can be single words or phrases/sentenses.
# -x X position.
# -y Y position.
# -gp Graphics parameters of texts.
# -... Pass to `textbox_grob`.
#
# == details
# All details can be found in the help page of `textbox_grob`.
#
grid.textbox = function(text, x = unit(0.5, "npc"), y = unit(0.5, "npc"), gp = gpar(), ...) {
	gb = textbox_grob(text, x = x, y = y, gp = gp, ...)
	grid.draw(gb)
}


# == title
# Text box annotations
#
# == param
# -align_to It controls how the text boxes are aligned to the heatmap rows. The value can be a categorical vector which have the same
#      length as heatmap rows, or a list of row indices. It does not necessarily include all row indices.
# -text The corresponding texts. The value should be a list of texts. To control graphics parameters of texts in the boxes, The value
#    of ``text`` can also be set as a list of data frames where the first column contains the text, from the second column contains
#    graphics parameters for each text. The column names should be "col", "fontsize", "fontfamily" and "fontface".
# -background_gp Graphics for the background.
# -which Only "row" is allowed.
# -by Are text boxed arranged by `anno_link` or by `anno_block`?
# -side Side of the annotation to the heatmap.
# -... Pass to `textbox_grob`.
#
# == example
# require(circlize)
# mat = matrix(rnorm(100*10), nrow = 100)
#
# split = sample(letters[1:10], 100, replace = TRUE)
# text = lapply(unique(split), function(x) {
# 	data.frame(month.name, col = rand_color(12, friendly = TRUE), fontsize = runif(12, 6, 14))
# })
# names(text) = unique(split)
#
# Heatmap(mat, cluster_rows = FALSE, row_split = split,
#     right_annotation = rowAnnotation(wc = anno_textbox(split, text))
# )
anno_textbox = function(align_to, text, background_gp = gpar(fill = "#DDDDDD", col = "#AAAAAA"),
	which = c("row", "column"), by = "anno_link", side = c("right", "left"), ...) {

	if(is.null(background_gp$fill)) background_gp$fill = "#DDDDDD"
	if(is.null(background_gp$col)) background_gp$col = "#AAAAAA"
	if(is.null(background_gp$lty)) background_gp$lty = 1
	if(is.null(background_gp$lwd)) background_gp$lwd = 1

	which = match.arg(which)[1]
	if(which == "column") {
		stop_wrap("`anno_textbox()` can only be used as row annotation.")
	}

	# 1. align_to numeric index, text: a data frame
	if(is.numeric(align_to) && (is.character(text) || is.data.frame(text))) {
		align_to = list(v = align_to)
		text = list(v = text)
	} else if(is.atomic(align_to) && is.list(text)) {
		align_to = split(seq_along(align_to), align_to)

		cn = intersect(names(align_to), names(text))
		if(length(cn) == 0) {
			stop_wrap("names of `text` should have overlap to levels in `align_to`.")
		} else {
			align_to = align_to[cn]
			text = text[cn]
		}
	} else if(is.list(align_to)) {
		if(!is.list(text)) {
			stop_wrap("Since `align_to` is a list, `text` should have the same format as `align_to`, which is a list. The elements in the list can either be character vectors or data frames that contain texts and graphics parameters.")
		} else {
			if(length(align_to) != length(text)) {
				stop_wrap("`align_to` and `text` should be two list with the same length.")
			}
		}
		if(!is.null(names(align_to)) && !is.null(names(text))) {
			if(length(setdiff(names(align_to), names(text))) == 0) {
				text = text[names(align_to)]
			} else {
				stop_wrap("Since `align_to` and `text` are all lists. They should have the same set of names.")
			}
		} else {
			stop_wrap("Since `align_to` and `text` are all lists. They should have the same set of names.")
		}
	} else {
		stop_wrap("Format of `align_to` or `text` is wrong.")
	}

	# a list of textbox grobs
	dev.null()
	
	gbl = lapply(text, function(x) {
		if(is.atomic(x)) {
			textbox_grob(text = x, background_gp = gpar(col = NA, fill = "transparent"), ...)
		} else if(is.data.frame(x)) {
			if("col" %in% colnames(x)) {
				col = x$col
			} else {
				col = get.gpar("col")[[1]]
			}

			if("fontsize" %in% colnames(x)) {
				fontsize = x$fontsize
			} else {
				fontsize = get.gpar("fontsize")[[1]]
			}

			if("fontfamily" %in% colnames(x)) {
				fontfamily = x$fontfamily
			} else {
				fontfamily = get.gpar("fontfamily")[[1]]
			}

			if("fontface" %in% colnames(x)) {
				fontface = x$fontface
			} else {
				fontface = get.gpar("fontface")[[1]]
			}
			textbox_grob(text = x[[1]], gp = gpar(col = col, fontsize = fontsize, fontfamily = fontfamily, fontface = fontface), 
				background_gp = gpar(col = NA, fill = "transparent"), ...)
		}
	})
	
	margin = unit(0, "pt")
	gbl_h = lapply(gbl, function(x) convertHeight(grobHeight(x), "cm") + margin)
	gbl_h = do.call(unit.c, gbl_h)

	gbl_w = lapply(gbl, function(x) convertWidth(grobWidth(x), "cm"))
	gbl_w = do.call(unit.c, gbl_w)
	gbl_w = max(gbl_w) + margin

	dev.off2()

	side = match.arg(side)[1]

	if(by %in% c("anno_link", "anno_zoom")) {
		panel_fun = function(index, nm) {
			pushViewport(viewport())
			grid.rect(gp = gpar(fill = background_gp$fill, col = background_gp$fill, lty = background_gp$lty, lwd = background_gp$lwd))
			if(side == "right") {
				grid.lines(c(0, 1, 1, 0), c(0, 0, 1, 1), gp = gpar(col = background_gp$col, lty = background_gp$lty, lwd = background_gp$lwd), default.units = "npc")
			} else {
				grid.lines(c(1, 0, 0, 1), c(0, 0, 1, 1), gp = gpar(col = background_gp$col, lty = background_gp$lty, lwd = background_gp$lwd), default.units = "npc")
			}
		    pushViewport(viewport(width = unit(1, "npc") - margin, height = unit(1, "npc") - margin))
		    gb = gbl[[nm]]
		    gb$vp$x = gb$vp$width*0.5
		    gb$vp$y = gb$vp$height*0.5
		    grid.draw(gb)
		    popViewport()
		    popViewport()
		}

		anno = anno_link(align_to = align_to, which = "row", panel_fun = panel_fun, 
	    	size = gbl_h, gap = unit(2, "mm"), width = gbl_w + unit(5, "mm"),
	    	link_gp = background_gp, internal_line = FALSE, side = side)
	} else {
		panel_fun = function(index, nm) {
			pushViewport(viewport())
		    grid.rect(gp = gpar(fill = background_gp$fill, col = background_gp$col, lty = background_gp$lty, lwd = background_gp$lwd))
			pushViewport(viewport(width = unit(1, "npc") - margin, height = unit(1, "npc") - margin))
		    gb = gbl[[nm]]
		    gb$vp$x = unit(0, "npc")
		    gb$vp$justification[1] = 0
		    gb$vp$valid.just[1] = 0
		    grid.draw(gb)
		    popViewport()
		    popViewport()
		}

		anno = anno_block(align_to = align_to, which = "row", panel_fun = panel_fun, 
	    	width = gbl_w + unit(5, "mm"))
	}

	anno
}

