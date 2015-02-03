setGeneric("draw", function(x, ...) {
	standardGeneric("draw")
})

setMethod("draw",
	c(x = "Heatmap"),
	function(x, ...) {
		ht_list = new("HeatmapList")
    	ht_list$add_heatmap(x)
		ht_list$draw(...)		
	}
)

setMethod("draw",
	c(x = "HeatmapList"),
	function(x, ...) {
		x$draw(...)		
	}
)
