
# a list of heatmaps
setRefClass("HeatmapList",
	fields = list(
		"list" = "list"
	)
)

# make some re-formatting for each heatmap
HeatmapList$methods(add_heatmap = function(ht) {
	
	# check settings of this new heatmap
	if(inherits(ht, "Heatmap")) {
		ht = list(ht)
		names(ht) = ht$name
	}

	# if ht is a heatmapList, all settings are already checked
	.self$list = c(.self$list, ht)
	return(.self)
})

# initialize the layout
heatmapList$methods(layout = function() {

	#global layout
	# main title
	# list of heatmaps
	# legend for annotation and matrix
	

})

heatmapList$methods(show = function() {

	})

"+.heatmapList" = function(ht1, ht2) {
	ht1$add_heatmap(ht2)
}