

# == title
# Global graphic options for heatmaps
#
# == param
# -... options, see 'details' section
# -RESET reset all the option values
# -READ.ONLY ``TRUE`` means only to return read-only values, ``FALSE`` means only to return non-read-only
#          values, ``NULL`` means to return both.
# -LOCAL switch local mode
# -ADD add new options
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
# -heatmap_legend_title_position set ``title_position`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_labels_gp set ``labels_gp`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_grid_width set ``grid_width`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_grid_height set ``grid_height`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_grid_border set ``grid_border`` element in ``heatmap_legend_param`` in `Heatmap`.
# -heatmap_legend_title_gp set ``title_gp`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_title_position set ``title_position`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_labels_gp set ``labels_gp`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_grid_width set ``grid_width`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_grid_height set ``grid_height`` element in ``legend_param`` in `SingleAnnotation`.
# -heatmap_legend_grid_border set ``grid_border`` element in ``legend_param`` in `SingleAnnotation`.
# -fast_hclust whether use `fastcluster::hclust` to speed up clustering?
#
# You can get or set option values by the traditional way (like `base::options`) or by ``$`` operator:
#
#     # to get option values
#     ht_opt("heatmap_row_names_gp")
#     ht_opt$heatmap_row_names_gp
#
#     # to set option values
#     ht_opt("heatmap_row_names_gp" = gpar(fontsize = 8))
#     ht_opt$heatmap_row_names_gp = gpar(fontsize = 8)
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
ht_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
ht_opt = setGlobalOptions(
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
	legend_title_gp = list(
		.value = NULL,
		.class = "gpar"),
	legend_title_position = list(
		.value = NULL,
		.class = "character"),
	legend_labels_gp = list(
		.value = NULL,
		.class = "gpar"),
	legend_grid_height = list(
		.value = NULL,
		.class = "unit"),
	legend_grid_width = list(
		.value = NULL,
		.class = "unit"),
	legend_grid_border = list(
		.value = NULL),

	border = list(
		.value = NULL),

	fast_hclust = list(
		.value = FALSE,
		.class = "logical",
		.length = 1
	),
	verbose = list(
		.value = FALSE,
		.class = "logical",
		.filter = function(x) {
			if(is.null(x)) {
				FALSE
			} else if(is.na(x)) {
				FALSE
			} else {
				x
			}
		},
		.length = 1),
	show_vp_border = FALSE,
	anno_simple_row_size = unit(5, "mm")
)


# == title
# Global graphic options for heatmaps
#
# == param
# -... options, see 'details' section
# -RESET reset all the option values
# -READ.ONLY ``TRUE`` means only to return read-only values, ``FALSE`` means only to return non-read-only
#          values, ``NULL`` means to return both.
# -LOCAL switch local mode
# -ADD add new options
#
# == details
# This function is deprecated. Please use `ht_opt` instead. However, changes by this function
# will also sychronized in `ht_opt`.
#
ht_global_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
ht_global_opt = ht_opt

.ENV = new.env()
.ENV$current_annotation_which = NULL
.ENV$row_order = NULL
.ENV$row_pos = NULL


DENDROGRAM_PADDING = unit(0.5, "mm")
DIMNAME_PADDING = unit(1, "mm")
TITLE_PADDING = unit(2.5, "mm")
COLUMN_ANNO_PADDING = unit(0.5, "mm")
ROW_ANNO_PADDING = unit(0.5, "mm")

GLOBAL_PADDING = unit(c(2, 2, 2, 2), "mm")

