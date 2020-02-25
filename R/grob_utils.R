# `[.grob` = function(x, i) {
# 	x2 = x
# 	for(nm in SUBSETABLE_FIELDS[[ intersect(names(SUBSETABLE_FIELDS), class(x)) ]]) {
# 		if(inherits(x2[[nm]], "gpar")) {
# 			# change to the class defined here
# 			class(x2[[nm]]) = "gpar"
# 		}

# 		if(length(x2[[nm]]) > 1) {
# 			x2[[nm]] = x2[[nm]][i]
# 		}
# 	}
# 	x2
# }

# `[.gpar` = function(x, i) {
# 	lapply(x, function(y) {
# 		if(length(y) > 1) {
# 			y[i]
# 		} else {
# 			y
# 		}
# 	})
# }

# SUBSETABLE_FIELDS = list(
# 	"text" = c("label", "x", "y", "gp"),
# 	"richtext_grob" = c("gp", "children", "childrenOrder")
# )

# length.text = function(x) {
# 	length(x$label)
# }

# length.richtext_grob = function(x) {
# 	length(x$children)
# }

# update_xy = function (gb, x, y, ...) {
# 	UseMethod("update_xy")
# }

# update_xy.text = function(gb, x, y, ...) {
# 	n = length(gb$label)
# 	if(!missing(x)) {
# 		if(n > 1 & length(x) > 1 && n != length(x)) {
# 			stop_wrap("Length of `x` should be the same as the length of labels.")
# 		}
# 		gb$x = x
# 	}
# 	if(!missing(y)) {
# 		if(n > 1 & length(y) > 1 && n != length(y)) {
# 			stop_wrap("Length of `y` should be the same as the length of labels.")
# 		}
# 		gb$y = y
# 	}
# 	gb
# }

# update_xy.richtext_grob = function(gb, x, y, ...) {
# 	n = length(gb$children)

# 	if(!missing(x)) {
# 		if(n > 1 & length(x) > 1 && n != length(x)) {
# 			stop_wrap("Length of `x` should be the same as the length of labels.")
# 		}
# 		for(i in 1:n) {
# 			if(length(x) == 1) {
# 				gb$children[[i]]$vp$x = x
# 			} else {
# 				gb$children[[i]]$vp$x = x[i]
# 			}
# 		}
# 	}
# 	if(!missing(y)) {
# 		if(n > 1 & length(y) > 1 && n != length(y)) {
# 			stop_wrap("Length of `y` should be the same as the length of labels.")
# 		}
# 		for(i in 1:n) {
# 			if(length(y) == 1) {
# 				gb$children[[i]]$vp$y = y
# 			} else {
# 				gb$children[[i]]$vp$y = y[i]
# 			}
# 		}
# 	}
# 	gb
# }

# textGrob = function(label, ...) {
# 	if(inherits(label, "grob")) {
# 		return(label)
# 	} else {
# 		grid::textGrob(label, ...)
# 	}
# }

# grid.text = function(label, x = unit(0.5, "npc"), y = unit(0.5, "npc"), ...) {
# 	if(inherits(label, "grob")) {
# 		gb = label
# 		gb = update_xy(gb, x, y)
# 		grid.draw(gb)
# 	} else {
# 		grid::grid.text(label, x, y, ...)
# 	}
# }
