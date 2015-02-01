
# a list of heatmaps
HeatmapList = setRefClass("HeatmapList",
	fields = list(
		"ht_list" = "list"
	)
)

# make some re-formatting for each heatmap
HeatmapList$methods(add_heatmap = function(ht) {
	
	# check settings of this new heatmap
	if(inherits(ht, "Heatmap")) {
		ht = list(ht)
		names(ht) = ht$name
	}

	# if ht is a HeatmapList, all settings are already checked
	.self$list = c(.self$ht_list, ht)
	return(.self)
})

# draw everthing
HeatmapList$methods(draw = function() {

	})

# initialize the layout
HeatmapList$methods(draw_heatmap_list = function() {

	#global layout
	# main title
	# list of heatmaps
	# legend for annotation and matrix
	
	n = length(.self$ht_list)

	nr = 9
	nc = 5 * n

	layout_height = unit.c(
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 1)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 2)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 3)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 4)))),
		unit(1, "null"),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 6)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 7)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 8)))),
		max(do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_height(k = 9))))
	)

	for(i in seq_len(n)) {
		.self$ht_list[[i]]$set_component_height(k = 1, layout_height[1])
		.self$ht_list[[i]]$set_component_height(k = 2, layout_height[2])
		.self$ht_list[[i]]$set_component_height(k = 3, layout_height[3])
		.self$ht_list[[i]]$set_component_height(k = 4, layout_height[4])
		.self$ht_list[[i]]$set_component_height(k = 6, layout_height[6])
		.self$ht_list[[i]]$set_component_height(k = 7, layout_height[7])
		.self$ht_list[[i]]$set_component_height(k = 8, layout_height[8])
		.self$ht_list[[i]]$set_component_height(k = 9, layout_height[9])
	}

	width_without_heatmap_body = do.call("unit.c", lapply(.self$ht_list, function(ht) ht$component_width(c(1:2, 3:4))))
	heatmap_ncol = sapply(.self$ht_list, function(ht) ncol(ht$matrix))
	heatmap_body_width = (unit(1, "npc") - sum(width_without_heatmap_body)) * 1/sum(heatmap_ncol) * heatmap_ncol

	layout_width = unit.c(width_without_heatmap_body[1:2], heatmap_body_width[1], width_without_heatmap_body[4:5])
	for(i in seq_len(n - 1) + 1) {
		layout_width = unit.c(layout_width, width_without_heatmap_body[5*(i-1) + 1:2], heatmap_body_width[i], width_without_heatmap_body[5*(i-1) + 4:5])
	}

	layout = grid.layout(nrow = 9, ncol = 5*n, widths = layout_width, heights = layout_height)
	pushViewport(viewport(layout = layout, name = "main_heatmap_list"))

	for(i in seq_len(n)) {
		# add each heatmap
		ht = ht_list[[i]]
		ht_layout_index = ht$layout_index
		ht_graphic_fun_list = ht$graphic_fun_list
		for(j in seq_len(nrow(ht_layout_index))) {
			pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2] + 5*(i-1)))
			ht_graphic_fun_list[[j]]()
			upViewport()
		}
	}

	upViewport()

})

HeatmapList$methods(draw_title = function(title, ...) {
	pushViewport(viewport(name = "main_title"))
	grid.text(title, gp = gpar(...))
	upViewport()
})

HeatmapList$methods(draw_legend = function() {

})

HeatmapList$methods(draw_annotation_legend = function() {

})

HeatmapList$methods(show = function() {

	})

"+.heatmapList" = function(ht1, ht2) {
	ht1$add_heatmap(ht2)
}