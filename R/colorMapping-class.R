
#####################################
# class to map values to colors
# generate legend and return size of the legend

colorMapping = setRefClass("colorMapping",
	fields = list(
		colors  = "character", # a list of colors
		levels  = "character", # levels which colors correspond to
		col_fun = "function", # function to map values to colors
		type    = "character",  # continuous or discrete
		name    = "character"
	)
)

# name is used as the title of the legend
colorMapping$methods(initialize = function(name, colors = NULL, levels = NULL, 
	col_fun = NULL, breaks = NULL) {

	if(is.null(name)) {
		stop("You should provide name.")
	}
	if(!is.null(colors) && !is.null(labels)) {
		if(length(colors) != length(levels)) {
			stop("length of colors and length of levels should be the same.\n")
		}
		colors <<- colors
		levels <<- levels
		names(colors) <<- levels
		type <<- "discrete"
	} else if(!is.null(col_fun)) {
		if(is.null(breaks)) {
			breaks = attr(col_fun, "breaks")
			if(is.null(breaks)) {
				stop("You should provide breaks.\n")
			}
		}
		le = grid.pretty(range(breaks))
		colors <<- col_fun(le)
		levels <<- as.character(le)
		col_fun <<- col_fun
		type <<- "continuous"
	} else {
		stop("initialization failed. Either specify `colors` + `levels` or `col_fun` + `breaks`\n")
	}

	name <<- name
})

colorMapping$methods(show = function(x) {
	if(.self$type == "discrete") {
		cat("Discrete color mapping:\n")
		cat("levels: ", .self$levels, "\n")
		cat("colors: ", .self$colors, "\n")
	} else if(.self$type == "continuous") {
		cat("Continuous color mapping:\n")
		cat("breaks:", .self$levels, "\n")
		cat("colors:", .self$colors, "\n")
	}
})

# map values to colors and keep the attributes
colorMapping$methods(map = function(x) {
	original_attr = attributes(x)
	if(.self$type == "discrete") {
		if(any(!x %in% .self$levels)) {
			stop("Cannot map some of the levels.")
		}
		x = .self$colors[x]
	} else {
		x = .self$col_fun(x)
	}

	# keep original attributes, such as dimension
	attributes(x) = original_attr
	return(x)
})

# add legend to the plot, or just return the size of the legend 
colorMapping$methods(legend = function(plot = TRUE) {

	legend_title_fontsize = 14
	legend_grid_height = unit(5, "mm")
	legend_grid_width = unit(5, "mm")
	legend_label_fontsize = 14

	# add title
	legend_title_grob = textGrob(.self$name, unit(0, "npc"), unit(1, "npc"), just = c("left", "top"), 
		gp = gpar(fontsize = legend_title_fontsize))
	legend_title_height = grobHeight(legend_title_grob)
	legend_title_width = grobWidth(legend_title_grob)

	nlevel = length(.self$levels)
	x = unit(rep(0, nlevel), "npc")
	y = unit(1, "npc") - 1.5*legend_title_height - (0:(nlevel-1))*(legend_grid_height + unit(1, "mm"))
	
	legend_label_max_width = max(do.call("unit.c", lapply(.self$levels, function(x) {
			grobWidth(textGrob(x, gp = gpar(fontsize = legend_label_fontsize)))
		})))
	vp_width = max(unit.c(legend_title_width, 
				   legend_grid_width + unit(2, "mm") + legend_label_max_width ))
	vp_height = legend_title_height*1.5 + nlevel*(legend_grid_height + unit(1, "mm"))

	if(plot) {
		pushViewport(viewport(width = vp_width, height = vp_height, name = paste0("legend_", .self$name)))
		grid.text(.self$name, unit(0, "npc"), unit(1, "npc"), just = c("left", "top"), 
			gp = gpar(fontsize = legend_title_fontsize))
		grid.rect(x, y,	width = legend_grid_width, height = legend_grid_height, just = c("left", "top"),
			gp = gpar(col = NA, fill = .self$colors))
		grid.text(.self$levels, x + legend_grid_width + unit(2, "mm"), y - legend_grid_height*0.5, just = c("left", "center"))
		upViewport()
	}

	return(unit.c(vp_width, vp_height))
})
