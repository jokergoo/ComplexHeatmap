

# == title
# Global Options for Heatmaps
#
# == param
# -... Options, see 'Details' section.
# -RESET Reset all the option values.
# -READ.ONLY Please ignore this argument.
# -LOCAL Please ignore this argument.
# -ADD Please ignore this argument.
#
# == details
# You can set some parameters for all heatmaps/annotations simultaneously by this global function.
# Pleast note you should put it before your heatmap code and reset
# all option values after drawing the heatmaps to get rid of affecting next heatmap.
#
# There are following parameters to control all heatmaps:
#
# -heatmap_row_names_gp set ``row_names_gp`` in all `Heatmap`.
# -heatmap_column_names_gp set ``column_names_gp`` in all `Heatmap`.
# -heatmap_row_title_gp set ``row_title_gp`` in all `Heatmap`.
# -heatmap_column_title_gp set ``column_title_gp`` in all `Heatmap`.
# -heatmap_border set ``border`` in all `Heatmap`.
#
# Following parameters control the legends: 
#
# -legend_title_gp set ``title_gp`` in all heatmap legends and annotation legends.
# -legend_title_position set ``title_position`` in all heatmap legends and annotation legends.
# -legend_labels_gp set ``labels_gp`` in all heatmap legends and annotation legends.
# -legend_grid_width set ``grid_width`` in all heatmap legends and annotation legends.
# -legend_grid_height set ``grid_height`` in all heatmap legends and annotation legends.
# -legend_border set ``border`` in all heatmap legends and annotation legends.
# -legend_gap Gap between legends. The value should be a vector of two units. One for gaps between
#         vertical legends and one for the horizontal legends. If only one single unit is specified,
#         the same gap set for the vertical and horizontal legends.
# -merge_legend wether merge heatmap and annotation legends.
#
# Following parameters control heatmap annotations:
#
# -annotation_border ``border`` in all `HeatmapAnnotation`.
# -simple_anno_size size for the simple annotation.
#
# Following parameters control the space between heatmap components:
#
# -DENDROGRAM_PADDING space bewteen dendrograms and heatmap body.
# -DIMNAME_PADDING space between row/column names and heatmap body.
# -TITLE_PADDING space between row/column titles and heatmap body. The value can have length of two which corresponds to the botton and top padding.
# -COLUMN_ANNO_PADDING space between column annotations and heatmap body.
# -ROW_ANNO_PADDING space between row annotations and heatmap body.
# -HEATMAP_LEGEND_PADDING space between heatmap legends and heatmaps
# -ANNOTATION_LEGEND_PADDING space between annotation legends and heatmaps
#
# Other parameters:
#
# -fast_hclust whether use `fastcluster::hclust` to speed up clustering?
# -show_parent_dend_line when heatmap is split, whether to add a dashed line to mark parent dendrogram and children dendrograms?
# -COLOR default colors for continuous color mapping.
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
# Reset to the default values by ``ht_opt(RESET = TRUE)``.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# ht_opt
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
	legend_border = list(
		.value = NULL),
	legend_gap = list(
		.value = unit(c(4, 4), "mm"),
		.length = 1:2,
		.class = "unit",
		.filter = function(x) {
			if(length(x) == 1) {
				rep(x, 2)
			} else {
				x
			}
		}
	),
	merge_legends = FALSE,

	heatmap_border = list(
		.value = NULL),
	annotation_border = list(
		.value = NULL),

	fast_hclust = list(
		.value = FALSE,
		.class = "logical",
		.length = 1
	),
	show_parent_dend_line = TRUE,

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
	message = list(
		.value = TRUE,
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
	show_vp = FALSE,
	simple_anno_size = list(
		.value = unit(5, "mm"),
		.class = "unit"
	),

	DENDROGRAM_PADDING = list(
		.value = unit(0.5, "mm"),
		.class = "unit"
	),
	DIMNAME_PADDING = list(
		.value = unit(1, "mm"),
		.class = "unit"
	),
	TITLE_PADDING = list(
		.value = NULL,
		.class = "unit",
		.filter = function(x) {
			if(length(x) == 1) {
				rep(x, 2)
			} else {
				x[1:2]
			}
		}
	),
	COLUMN_ANNO_PADDING = list(
		.value = unit(1, "mm"),
		.class = "unit"
	),
	ROW_ANNO_PADDING = list(
		.value = unit(1, "mm"),
		.class = "unit"
	),
	HEATMAP_LEGEND_PADDING = list(
		.value = unit(2, "mm"),
		.class = "unit"
	),
	ANNOTATION_LEGEND_PADDING = list(
		.value = unit(2, "mm"),
		.class = "unit"
	),

	### invisible
	"__export_image_size__" = list(
		.visible = FALSE,
		.value = FALSE
	),
	save_last = list(
		.value = FALSE
	),
	"validate_names" = TRUE,
	raster_temp_image_max_width = 30000,
	raster_temp_image_max_height = 30000,
	COLOR = c("blue", "#EEEEEE", "red")
)


# == title
# Global Options for Heatmaps
#
# == param
# -... Options.
# -RESET Reset all the option values.
# -READ.ONLY ``TRUE`` means only to return read-only values, ``FALSE`` means only to return non-read-only
#          values, ``NULL`` means to return both.
# -LOCAL Wwitch to local mode.
# -ADD Add new options.
#
# == details
# This function is deprecated. Please use `ht_opt` instead. However, changes by this function
# will also be sychronized in `ht_opt`.
#
ht_global_opt = function(..., RESET = FALSE, READ.ONLY = NULL, LOCAL = FALSE, ADD = FALSE) {}
ht_global_opt = ht_opt

.ENV = new.env(parent = emptyenv())
.ENV$current_annotation_which = NULL
.ENV$row_order = NULL
.ENV$row_pos = NULL
.ENV$last = NULL

# DENDROGRAM_PADDING = unit(0.5, "mm")
# DIMNAME_PADDING = unit(1, "mm")
# TITLE_PADDING = unit(2.5, "mm")
# COLUMN_ANNO_PADDING = unit(1, "mm")
# ROW_ANNO_PADDING = unit(1, "mm")

GLOBAL_PADDING = unit(rep(5.5, 4), "points")


is_under_jupyter = function() {
    "IRkernel" %in% loadedNamespaces()
}

.ENV$IS_UNDER_JUPYTER = FALSE
.ENV$IS_UNDER_JUPYTER_IGNORE = FALSE


