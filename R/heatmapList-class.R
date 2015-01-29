
# a list of heatmaps
setRefClass("heatmapList",
	fields = list(
		"list" = "list"
	)
)

# make some re-formatting for each heatmap
heatmapList$methods(add_heatmap = function(ht) {
	
	# check settings of this new heatmap
	if(inherits(ht, "heatmap")) {

		# whether it is the first one
		# check show rownames, ...

		ht = list(ht)
	}

	# if ht is a heatmapList, all settings are already checked
	.self$list = c(.self$list, ht)
	return(.self)
})

# initialize the layout
heatmapList$methods(layout)

heatmapList$methods(show = function() {

	})

"+.heatmapList" = function(ht1, ht2) {
	ht1$add_heatmap(ht2)
}