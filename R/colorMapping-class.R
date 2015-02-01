
#####################################
# class to map values to colors
#
# 1. map values to colors, both discrete and continuous 
# 2. generate a legend which corresponds to color mapping
# 3. return the size of the legend

ColorMapping = setRefClass("ColorMapping",
	fields = list(
		colors  = "character", # a list of colors
		levels  = "character", # levels which colors correspond to
		col_fun = "function", # function to map values to colors
		type    = "character",  # continuous or discrete
		name    = "character"  # used to map to the dataset and taken as the title of the legend
	)
)

# constructor
ColorMapping$methods(initialize = function(name, colors = NULL, levels = NULL, 
	col_fun = NULL, breaks = NULL) {

	if(is.null(name)) {
		stop("You should provide name.")
	}
	if(!is.null(colors)) {
		if(is.null(levels)) {
			if(is.null(names(colors))) {
				stop("either provide `levels` or provide `colors` with names.\n")
			}
			levels = names(colors)
		}
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
		stop("initialization failed. Either specify `colors` + `levels` or `col_fun` + `breaks` (optional)\n")
	}

	name <<- name

	return(invisible(.self))
})

ColorMapping$methods(show = function(x) {
	if(.self$type == "discrete") {
		cat("Discrete color mapping:\n")
		cat("levels:\n")
		print(.self$levels)
		cat("\n")
		cat("colors:\n")
		col = .self$colors; names(col) = NULL
		print(col)
		cat("\n")
	} else if(.self$type == "continuous") {
		cat("Continuous color mapping:\n")
		cat("breaks:\n")
		print(.self$levels)
		cat("\n")
		cat("colors:\n")
		col = .self$colors; names(col) = NULL
		print(col)
		cat("\n")
	}
})

# map values to colors and keep the attributes (in case it is a matrix)
ColorMapping$methods(map = function(x) {
	original_attr = attributes(x)
	if(.self$type == "discrete") {
		if(any(! x %in% .self$levels)) {
			msg = paste0("Cannot map some of the levels:\n", paste(setdiff(x, .self$levels), sep = ", ", collapse = ", "))
			stop(msg)
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
ColorMapping$methods(legend = function(plot = TRUE) {

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
