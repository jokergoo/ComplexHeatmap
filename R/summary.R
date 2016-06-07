summary.Heatmap = function(object, ...) {
	ht_list = new("HeatmapList")
    ht_list = add_heatmap(ht_list, object)
    summary(ht_list)
}

summary.HeatmapList = function(object, ...) {
	
}

summary_single_heatmap = function(ht) {
	cat("name:", ht@name, "\n")
}

sumamry_row_annotation = function(ha) {

}