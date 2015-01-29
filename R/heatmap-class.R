
###############################
# class for single heatmap

setRefClass("heatmap",
    fields = list(
    	"matrix" = "matrix",  # original order
    	"hclust_row" = "hclust",
    	"hclust_col" = "hclust",
    	"column_anno" = "ANY", # annotation data frame, order of columns are same as matrix
    	"column_anno_color_mapping" = "list", # a list of colorMapping class objects
    	"matrix_color_mapping" = "colorMapping")

heatmap$methods(initialize = function(matrix, order_row_by = "hclust", order_col_by = "hclust", km = 1,
	annotation, annotation_color, ...) {

})

heatmap$methods(nrow = function() {
	nrow(.self$matrix)
})

heatmap$methods(ncol = function() {
	ncol(.self$matrix)
})

heatmap$methods(nanno = function() {

	})


heatmap$methods(add_heatmap = function(ht) {
	ht_list = new("heatmapList")
	ht_list$add_heatmap(.self)
	ht_list$add_heatmap(ht)
})

heatmap$methods(heatmapBodyGrob)
heatmap$methods(hclustGrob)
heatmap$methods(annotationGrob)
heatmap$methods(dimnamesGrob)


"+.heatmap" = function(ht1, ht2) {
	ht1$add_heatmap(ht2)
}

