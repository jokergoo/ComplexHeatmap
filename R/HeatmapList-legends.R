
# == title
# Draw legends for all heatmaps
#
# == param
# -object a `HeatmapList-class` object
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == details
# A viewport is created which contains heatmap legends.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_legend",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    # actually we call the "heatmap legends" the main legends
    # for horizontal heamtap list, legends of heamtap/row annotation/left/right annotation
    # for vertical heatmap list, legends of heamtap/column annotation/top/bottom annotation
    # if merge_legends is true, the it contains all the legends

    side = object@heatmap_legend_param$side
    size = object@heatmap_legend_param$size
    padding = object@heatmap_legend_param$padding
    direction = object@direction

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
            }
        } else {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(object@ht_list_param$merge_legends && !is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
            }
        }
    }

    annotation_legend_side = object@annotation_legend_param$side
    annotation_legend_size = object@annotation_legend_param$size
    offset = object@heatmap_legend_param$offset

    if(inherits(offset, "character")) {
        if(offset == "annotation_top") {
            i_ht = object@ht_list_param$main_heatmap
            main_ht = object@ht_list[[i_ht]]
            offset = unit(1, "npc") - main_ht@layout$layout_size$column_title_top_height - 
                main_ht@layout$layout_size$column_dend_top_height - 
                (unit(0.5, "npc") + size[2]*0.5)
        } 
    }
    offset = unit(0, "mm")

    if(side != annotation_legend_side) {
        y = unit(0.5, "npc")
        pushViewport(viewport(name = "heatmap_legend", x = unit(0.5, "npc"), y = y + offset, width = size[1], height = size[2], just = c("center", "center")))
    } else {
        if(side %in% c("left", "right")) {
            y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
            y2 = unit(0.5, "npc") + annotation_legend_size[2]*0.5
            y = max(y1, y2)
            pushViewport(viewport(name = "heatmap_legend", x = unit(0.5, "npc"), y = y + offset, width = size[1], height = size[2], just = c("center", "top")))           
        } else {
            x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
            x2 = unit(0.5, "npc") - annotation_legend_size[1]*0.5
            x = min(x1, x2)
            pushViewport(viewport(name = "heatmap_legend", x = x, y = unit(0.5, "npc") + offset, width = size[1], height = size[2], just = c("left", "center")))           
        }
    }
    draw_legend(ColorMappingList, ColorMappingParamList, side = side, legend_list = legend_list, padding = padding, ...)

    upViewport()
})

# == title
# Draw legends for all column annotations
#
# == param
# -object a `HeatmapList-class` object
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == details
# A viewport is created which contains annotation legends.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_annotation_legend",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    # actually we call the "annotation legends" the secondary legends
    # for horizontal heamtap list, legends of all top/bottom annotations
    # for vertical heatmap list, legends of all left/right annotations

    side = object@annotation_legend_param$side
    size = object@annotation_legend_param$size
    padding = object@annotation_legend_param$padding
    offset = object@annotation_legend_param$offset
    direction = object@direction

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
            }
        } else {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            }
        }
    }

    heatmap_legend_side = object@heatmap_legend_param$side
    heatmap_legend_size = object@heatmap_legend_param$size
    if(side != heatmap_legend_side) {
        pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc") + offset, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("center", "center")))
    } else {
        if(side %in% c("left", "right")) {
            y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
            y2 = unit(0.5, "npc") + heatmap_legend_size[2]*0.5
            y = max(y1, y2)
            pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc") + offset, y = y, width = size[1], height = size[2], just = c("center", "top")))           
        } else {
            x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
            x2 = unit(0.5, "npc") - heatmap_legend_size[1]*0.5
            x = min(x1, x2)
            pushViewport(viewport(name = "annotation_legend", x = x + offset, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("left", "center")))           
        }
    }

    draw_legend(ColorMappingList, ColorMappingParamList, side = side, legend_list = legend_list, padding = padding, ...)
    upViewport()
})

# == title
# Size of the heatmap legend viewport
#
# == param
# -object a `HeatmapList-class` object
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == detail
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "heatmap_legend_size",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    side = object@heatmap_legend_param$side
    padding = object@heatmap_legend_param$padding
    direction = object@direction

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
            }
        } else {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(object@ht_list_param$merge_legends && !is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
                if(object@ht_list_param$merge_legends && !is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
            }
        }
    }

    size = draw_legend(ColorMappingList, ColorMappingParamList, side = side, plot = FALSE, legend_list = legend_list, padding = padding, ...)

    return(size)
})

# == title
# Size of the annotation legend viewport
#
# == param
# -object a `HeatmapList-class` object.
# -legend_list a list of self-defined legend, should be wrapped into `grid::grob` objects.
# -... graphic parameters passed to `color_mapping_legend,ColorMapping-method`.
#
# == detail
# Legends for all heatmaps or legends for all annotations will be put in one viewport. This function
# calculates the size of such viewport. Note graphic parameters for legends will affect the size.
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "annotation_legend_size",
    signature = "HeatmapList",
    definition = function(object, legend_list = list(), ...) {

    side = object@annotation_legend_param$side
    padding = object@annotation_legend_param$padding
    direction = object@direction

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
            }
        } else {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            }
        }
    }

    size = draw_legend(ColorMappingList, ColorMappingParamList, side = side, plot = FALSE, legend_list = legend_list, padding = padding, ...)
    
    return(size)
})

# create a viewport which contains legend
# currently, one-row or one-column legend is supported
draw_legend = function(ColorMappingList, ColorMappingParamList, side = c("right", "left", "top", "bottom"), 
    plot = TRUE, gap = unit(2, "mm"), legend_list = list(), padding = unit(c(0, 0, 0, 0), "mm"), 
    max_height = unit(dev.size("cm")[2], "cm"), max_width = unit(dev.size("cm")[1], "cm"), ...) {

    side = match.arg(side)[1]

    # remove legends which are duplicated by testing the names
    legend_names = sapply(ColorMappingList, function(x) x@name)
    l = !duplicated(legend_names)
    ColorMappingList = ColorMappingList[l]
    ColorMappingParamList = ColorMappingParamList[l]

    n = length(ColorMappingList)
    if(n == 0 && length(legend_list) == 0) {
        return(unit(c(0, 0), "mm"))
    } else {
        cm_grob = c(lapply(seq_along(ColorMappingList), function(i) color_mapping_legend(ColorMappingList[[i]], param = ColorMappingParamList[[i]], plot = FALSE, ...)), legend_list)
        if(side %in% c("left", "right")) {
            pk = packLegend(list = cm_grob, gap = gap, direction = "vertical", max_height = max_height)  
        } else {
            pk = packLegend(list = cm_grob, gap = gap, direction = "horizontal", max_width = max_width)
        }
    }

    # width = convertWidth(grobWidth(pk), "mm")
    # height = convertHeight(grobHeight(pk), "mm")
    width = width(pk)
    height = height(pk)

    if(plot) {
        if(side == "right") {
            draw(pk, x = unit(1, "npc"), just = "right")
        } else if(side == "left") {
            draw(pk, x = unit(0, "npc"), just = "left")
        } else if(side == "top") {
            draw(pk, y = unit(1, "npc"), just = "top")
        } else if(side == "bottom") {
            draw(pk, y = unit(0, "npc"), just = "bottom")
        }
    }

    size = unit.c(width, height)

    size = unit.c(size[1] + padding[2] + padding[4], size[2] + padding[1] + padding[3])
    return(size)
}
