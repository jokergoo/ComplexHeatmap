# common class for both row annotation and column annotation

HeatmapAnnotation = setClass("HeatmapAnnotation",
	slots = list(
		list = "list"
	)
)

HeatmapAnnotation(df, col, type, height)
HeatmapAnnotation(fun1, df)
HeatmapAnnotation(fun1, df, fun2, )

setMethod(f = "initialize",
	signature = "HeatmapAnnotation",
	definition = function(.Object, df, col, type, height,gp_list, ...) {

		if(!missing(df)) {
			for(i in seq_len(ncol(df))) {
				.Object@list[[i]]$name
				.Object@list[[i]]$value
				.Object@list[[i]]$color_mapping
				.Object@list[[i]]$height
				.Object@list[[i]]$fun
				.Object@list[[i]]$show_legend = 

				if(type[i] %in% "heatmap") {
					.Object@list[[i]]$fun = function(index) {
						n = length(index)
						x = (seq_len(n) - 0.5) / n
						fill = map(cm[index], object@column_anno[[i]])
						grid.rect(x, y = 0.5, width = 1/n, height = 1, gp = gpar(fill = fill))
					}
				}
			}
		}
		fun_list = as.list(...)
		if(length(fun_list)) {
			fun_name = names(fun_list)
			.Ojbect@list[[i]]$name
			.Object@list[[i]]$height

		}
})

setMethod(f = "get_color_mapping_list",
	signature = "HeatmapAnnotation",
	definition = function(object) {

		})

setMethod(f = "draw",
	signature = "HeatmapAnnotation",
	definition = function(object, index) {

	pushViewport(viewport())
	for(i in seq_len(object@list)) {
		object@list[[i]]$fun(index)
	}

})