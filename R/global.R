

# == title
# Global graphic options for heatmaps
#
# == param
# -... options, see 'details' section
# -RESET reset all the option values
# -READ.ONLY ``TRUE`` means only to return read-only values, ``FALSE`` means only to return non-read-only
#          values, ``NULL`` means to return both.
# -LOCAL switch local mode
#
# == details
# You can set some parameters for all heatmaps/annotations simultaneously by this global function.
# Pleast note you should put it before your heatmap code and reset
# all option values after drawing the heatmaps to get rid of affecting next heatmap plotting.
#
# There are following parameters:
#
# -heatmap_row_names_gp set ``row_names_gp`` in `Heatmap`.
# -heatmap_column_names_gp set ``column_names_gp`` in `Heatmap`.
# -heatmap_row_title_gp set ``row_title_gp`` in `Heatmap`.
# -heatmap_column_title_gp set ``column_title_gp`` in `Heatmap`.
# -heatmap_legend_title_gp set ``title_gp`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_labels_gp set ``labels_gp`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_grid_width set ``grid_width`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_grid_height set ``grid_height`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_grid_border set ``grid_border`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_title_gp set ``title_gp`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_labels_gp set ``labels_gp`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_grid_width set ``grid_width`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_grid_height set ``grid_height`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_grid_border set ``grid_border`` element in ``legend_param`` in `SingleAnnotation`.
#
# You can get or set option values by the traditional way (like `base::options`) or by ``$`` operator:
#
#     # to get option values
#     ht_global_opt("heatmap_row_names_gp")
#     ht_global_opt$heatmap_row_names_gp
#
#     # to set option values
#     ht_global_opt("heatmap_row_names_gp" = gpar(fontsize = 8))
#     ht_global_opt$heatmap_row_names_gp = gpar(fontsize = 8)
#
#
# == value
# Depends on the options users selected.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# # no example for this function
# NULL
#
ht_global_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE) {}
ht_global_opt = setGlobalOptions(
	heatmap_row_names_gp = list(
		.value = NULL,
		.class = "gpar"),
	heatmap_column_names_gp = list(
		.value = NULL,
		.class = "gpar"),
	heatmap_row_title_gp = list(
		.value = NULL,
		.class = "gpar"),
	heatmap_column_title_gp = list(
		.value = NULL,
		.class = "gpar"),
	heatmap_legend_title_gp = list(
		.value = NULL,
		.class = "gpar"),
	heatmap_legend_labels_gp = list(
		.value = NULL,
		.class = "gpar"),
	heatmap_legend_grid_height = list(
		.value = NULL,
		.class = "unit"),
	heatmap_legend_grid_width = list(
		.value = NULL,
		.class = "unit"),
	heatmap_legend_grid_border = list(
		.value = NULL),

	annotation_legend_title_gp = list(
		.value = NULL,
		.class = "gpar"),
	annotation_legend_labels_gp = list(
		.value = NULL,
		.class = "gpar"),
	annotation_legend_grid_height = list(
		.value = NULL,
		.class = "unit"),
	annotation_legend_grid_width = list(
		.value = NULL,
		.class = "unit"),
	annotation_legend_grid_border = list(
		.value = NULL)
)
