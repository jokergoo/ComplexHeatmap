
# == title
# Draw legends for All Heatmaps
#
# == param
# -object A `HeatmapList-class` object.
# -legend_list A list of self-defined legends, should be wrapped into `grid::grob` objects. It is
#          normally constructed by `Legend`.
# -... Other arguments.
#
# == details
# Actually we call the "heatmap legends" as the main legends.
# For horizontal heatmap list, the legends are those from heamtap/row annotation/left/right annotation.
# For vertical heatmap list, the legends are those from heamtap/column annotation/top/bottom annotation.
# if ``merge_legends`` is true in `draw,HeatmapList-method`, then it contains all legends shown on the plot.
#
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

    # if legend_grouping == "adjusted"
    #   actually we call the "heatmap legends" the main legends
    #   for horizontal heamtap list, legends of heamtap/row annotation/left/right annotation
    #   for vertical heatmap list, legends of heamtap/column annotation/top/bottom annotation
    #   if merge_legends is true, the it contains all the legends

    side = object@heatmap_legend_param$side
    size = object@heatmap_legend_param$size
    padding = object@heatmap_legend_param$padding
    direction = object@direction
    legend_grouping = object@ht_list_param$legend_grouping

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                    }
                }
                if(!is.null(ht@top_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                    }
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(!is.null(ht@bottom_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                    }
                }
                if(!is.null(ht@right_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                    }
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
                }
            }
        } else {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                    }
                }
                if(!is.null(ht@top_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                    }
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(!is.null(ht@bottom_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                    }
                }
                if(!is.null(ht@right_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                    }
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
                }
            }
        }
    }

    annotation_legend_side = object@annotation_legend_param$side
    annotation_legend_size = object@annotation_legend_param$size
    align_legend = object@heatmap_legend_param$align_legend
    if(is.null(align_legend)) {
        align_legend = guess_align_legend(object,
            object@heatmap_legend_param$size, object@annotation_legend_param$size,
            object@heatmap_legend_param$side, object@annotation_legend_param$side,
            test_on = "heatmap_legend")
    }

    if(align_legend %in% c("global_center", "global-center")) {
        if(side != annotation_legend_side) {
            y = unit(0.5, "npc")
            pushViewport(viewport(name = "heatmap_legend", x = unit(0.5, "npc"), y = y, width = size[1], height = size[2], just = c("center", "center")))
        } else {
            if(side %in% c("left", "right")) {
                y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
                y2 = unit(0.5, "npc") + annotation_legend_size[2]*0.5
                y = max(y1, y2)
                pushViewport(viewport(name = "heatmap_legend", x = unit(0.5, "npc"), y = y, width = size[1], height = size[2], just = c("center", "top")))           
            } else {
                x1 = unit(0.5, "npc") - size[1]*0.5  # left of heatmap legend
                x2 = unit(0.5, "npc") - annotation_legend_size[1]*0.5
                x = min(x1, x2)
                pushViewport(viewport(name = "heatmap_legend", x = x, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("left", "center")))           
            }
        }
    } else {
   
        if(align_legend %in% c("heatmap_top", "heatmap-top")) {
            if(!side %in% c("left", "right")) {
                stop_wrap("Heatmap legends should be put on the left or right side of the heatmaps if `align_legend` is 'heatmap_top'.")
            }
            if(object@direction == "horizontal") {
                top_h = sum(component_height(object@ht_list[[ which_main_ht(object) ]], 1:4))
            } else {
                top_h = calc_before_h(object)
            }
            y = unit(1, "npc") - top_h
            legend_just = "top"
            x = unit(0.5, "npc")
        } else if(align_legend %in% c("heatmap_left", "heatmap-left")) {
            if(!side %in% c("top", "bottom")) {
                stop_wrap("Heatmap legends should be put on the top or bottom side of the heatmaps if `align_legend` is 'heatmap_left'.")
            }
            if(object@direction == "vertical") {
                left_w = sum(component_width(object@ht_list[[ which_main_ht(object) ]], 1:4))
            } else {
                left_w = calc_before_w(object)
            }
            x = left_w
            legend_just = "left"
            y = unit(0.5, "npc")
        } else if(align_legend %in% c("heatmap_center", "heatmap-center")) {
            if(side %in% c("left", "right")) {
                if(object@direction == "horizontal") {
                    bottom_h = sum(component_height(object@ht_list[[ which_main_ht(object) ]], 6:9))
                    top_h = sum(component_height(object@ht_list[[ which_main_ht(object) ]], 1:4))
                    anno_bottom_extend = do.call("unit.c", lapply(object@ht_list, function(ht) {
                        if(inherits(ht, "HeatmapAnnotation")) {
                            ht@extended[1]
                        } else {
                            if(!is.null(ht@left_annotation)) {
                                u1 = ht@left_annotation@extended[1]
                            } else {
                                u1 = unit(0, "mm")
                            }
                            if(!is.null(ht@right_annotation)) {
                                u2 = ht@right_annotation@extended[1]
                            } else {
                                u2 = unit(0, "mm")
                            }
                            unit.c(u1, u2)
                        }
                    }))
                    bottom_h = max(unit.c(bottom_h, anno_bottom_extend))
                    bottom_h = convertHeight(bottom_h, "mm")
                } else {
                    bottom_h = calc_after_h(object)
                    top_h = calc_before_h(object)
                }
                ht_h = unit(1, "npc") - top_h - bottom_h
                y = bottom_h + ht_h*0.5
                legend_just = "center"
                x = unit(0.5, "npc")
                # grid.rect(y = y, height = ht_h)
            } else {
                if(object@direction == "horizontal") {
                    left_w = calc_before_w(object)
                    right_w = calc_after_w(object)
                } else {
                    left_w = sum(component_width(object@ht_list[[ which_main_ht(object) ]], 1:4))
                    right_w = sum(component_width(object@ht_list[[ which_main_ht(object) ]], 6:9))
                }
                ht_w = unit(1, "npc") - left_w - right_w
                x = left_w + ht_w*0.5
                legend_just = "center"
                y = unit(0.5, "npc")
                # grid.rect(x = x, width = ht_w)
            }
        } else {
            stop_wrap("Wrong value for `align_heatmap_legend`.")
        }

        if(side != annotation_legend_side) {
            pushViewport(viewport(name = "heatmap_legend", x = x, y = y, width = size[1], height = size[2], just = legend_just))           
        } else {
            if(side %in% c("left", "right")) {
                if(align_legend %in% c("heatmap_center", "heatmap-center")) {
                    y = bottom_h + ht_h*0.5 + max(size[2]*0.5, annotation_legend_size[2]*0.5)
                    legend_just = "top"
                }
                pushViewport(viewport(name = "heatmap_legend", x = x, y = y, width = size[1], height = size[2], just = legend_just))  
            } else {
                if(align_legend %in% c("heatmap_center", "heatmap-center")) {
                    x = left_w + ht_w*0.5 - max(size[1]*0.5, annotation_legend_size[1]*0.5)
                    legend_just = "left"
                }
                pushViewport(viewport(name = "heatmap_legend", x = x, y = y, width = size[1], height = size[2], just = legend_just))  
            }
        }   
    }
    draw_legend(ColorMappingList, ColorMappingParamList, side = side, legend_list = legend_list, padding = padding, ...)

    upViewport()
})

# == title
# Draw legends for All Annotations
#
# == param
# -object A `HeatmapList-class` object.
# -legend_list A list of self-defined legends, should be wrapped into `grid::grob` objects.
#      It is normally constructed by `Legend`.
# -... Other arguments.
#
# == details
# We call the "annotation legends" as the secondary legends.
# For horizontal heamtap list, the legends are those from all top/bottom annotations, and for vertical heatmap list, 
# the legends are those from all left/right annotations.
#
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
    legend_grouping = object@ht_list_param$legend_grouping

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                    }
                }
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                    }
                }
            } else if(inherits(ht, "HeatmapAnnotation")) {
                if(legend_grouping == "original") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_color_mapping_list(ht))
                }
            }
        } else {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(!is.null(ht@top_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                    }
                }
                if(!is.null(ht@bottom_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                    }
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            } else if(inherits(ht, "HeatmapAnnotation")) {
                if(legend_grouping == "original") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_color_mapping_list(ht))
                }
            }
        }
    }

    heatmap_legend_side = object@heatmap_legend_param$side
    heatmap_legend_size = object@heatmap_legend_param$size
    align_legend = object@annotation_legend_param$align_legend
    if(is.null(align_legend)) {
        align_legend = guess_align_legend(object,
            object@heatmap_legend_param$size, object@annotation_legend_param$size,
            object@heatmap_legend_param$side, object@annotation_legend_param$side,
            test_on = "annotation_legend")
    }

    if(align_legend %in% c("global_center", "global-center")) {
        if(side != heatmap_legend_side) {
            pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc"), y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("center", "center")))
        } else {
            if(side %in% c("left", "right")) {
                y1 = unit(0.5, "npc") + size[2]*0.5  # top of heatmap legend
                y2 = unit(0.5, "npc") + heatmap_legend_size[2]*0.5
                y = max(y1, y2)
                pushViewport(viewport(name = "annotation_legend", x = unit(0.5, "npc"), y = y, width = size[1], height = size[2], just = c("center", "top")))           
            } else {
                x1 = unit(0.5, "npc") - size[1]*0.5  # top of heatmap legend
                x2 = unit(0.5, "npc") - heatmap_legend_size[1]*0.5
                x = min(x1, x2)
                pushViewport(viewport(name = "annotation_legend", x = x, y = unit(0.5, "npc"), width = size[1], height = size[2], just = c("left", "center")))           
            }
        }
    } else {
        if(align_legend %in% c("heatmap_top", "heatmap-top")) {
            if(!side %in% c("left", "right")) {
                stop_wrap("Annotation legends should be put on the left or right side of the heatmaps if `align_legend` is 'heatmap_top'.")
            }
            if(object@direction == "horizontal") {
                top_h = sum(component_height(object@ht_list[[ which_main_ht(object) ]], 1:4))
            } else {
                top_h = calc_before_h(object)
            }
            y = unit(1, "npc") - top_h
            legend_just = "top"
            x = unit(0.5, "npc")
        } else if(align_legend %in% c("heatmap_left", "heatmap-left")) {
            if(!side %in% c("top", "bottom")) {
                stop_wrap("Annotation legends should be put on the top or bottom side of the heatmaps if `align_legend` is 'heatmap_left'.")
            }
            if(object@direction == "vertical") {
                left_w = sum(component_width(object@ht_list[[ which_main_ht(object) ]], 1:4))
            } else {
                left_w = calc_before_w(object)
            }
            x = left_w
            legend_just = "left"
            y = unit(0.5, "npc")
        } else if(align_legend %in% c("heatmap_center", "heatmap-center")) {
            if(side %in% c("left", "right")) {
                if(object@direction == "horizontal") {
                    bottom_h = sum(component_height(object@ht_list[[ which_main_ht(object) ]], 6:9))
                    top_h = sum(component_height(object@ht_list[[ which_main_ht(object) ]], 1:4))
                    anno_bottom_extend = do.call("unit.c", lapply(object@ht_list, function(ht) {
                        if(inherits(ht, "HeatmapAnnotation")) {
                            ht@extended[1]
                        } else {
                            if(!is.null(ht@left_annotation)) {
                                u1 = ht@left_annotation@extended[1]
                            } else {
                                u1 = unit(0, "mm")
                            }
                            if(!is.null(ht@right_annotation)) {
                                u2 = ht@right_annotation@extended[1]
                            } else {
                                u2 = unit(0, "mm")
                            }
                            unit.c(u1, u2)
                        }
                    }))
                    bottom_h = max(unit.c(bottom_h, anno_bottom_extend))
                    bottom_h = convertHeight(bottom_h, "mm")
                } else {
                    bottom_h = calc_after_h(object)
                    top_h = calc_before_h(object)
                }
                ht_h = unit(1, "npc") - top_h - bottom_h
                y = bottom_h + ht_h*0.5
                legend_just = "center"
                x = unit(0.5, "npc")
                # grid.rect(y = y, height = ht_h)
            } else {
                if(object@direction == "horizontal") {
                    left_w = calc_before_w(object)
                    right_w = calc_after_w(object)
                } else {
                    left_w = sum(component_width(object@ht_list[[ which_main_ht(object) ]], 1:4))
                    right_w = sum(component_width(object@ht_list[[ which_main_ht(object) ]], 6:9))
                }
                ht_w = unit(1, "npc") - left_w - right_w
                x = left_w + ht_w*0.5
                legend_just = "center"
                y = unit(0.5, "npc")
                # grid.rect(x = x, width = ht_w)
            }
        } else {
            stop_wrap("Wrong value for `align_annotation_legend`.")
        }

        if(side != heatmap_legend_side) {
            pushViewport(viewport(name = "annotation_legend", x = x, y = y, width = size[1], height = size[2], just = legend_just))           
        } else {
            if(side %in% c("left", "right")) {
                if(align_legend %in% c("heatmap_center", "heatmap-center")) {
                    y = bottom_h + ht_h*0.5 + max(size[2]*0.5, heatmap_legend_size[2]*0.5)
                    legend_just = "top"
                }
                pushViewport(viewport(name = "annotation_legend", x = x, y = y, width = size[1], height = size[2], just = legend_just))  
            } else {
                if(align_legend %in% c("heatmap_center", "heatmap-center")) {
                    x = left_w + ht_w*0.5 - max(size[1]*0.5, heatmap_legend_size[1]*0.5)
                    legend_just = "left"
                }
                pushViewport(viewport(name = "annotation_legend", x = x, y = y, width = size[1], height = size[2], just = legend_just))  
            }
        }   
    }

    draw_legend(ColorMappingList, ColorMappingParamList, side = side, legend_list = legend_list, padding = padding, ...)
    upViewport()
})

calc_ht_h = function(ht_list, inlcude_bottom = FALSE) {
    if(ht_list@direction == "horizontal") {
        bottom_h = sum(component_height(ht_list@ht_list[[ which_main_ht(ht_list) ]], 6:9))
        top_h = sum(component_height(ht_list@ht_list[[ which_main_ht(ht_list) ]], 1:4))
    } else {
        bottom_h = calc_after_h(ht_list)
        top_h = calc_before_h(ht_list)
    }
    ht_h = unit(1, "npc") - top_h - bottom_h
    if(inlcude_bottom) {
        ht_h = unit(1, "npc") - top_h
    }
    ht_h
}

calc_ht_w = function(ht_list, include_right = FALSE) {
    if(ht_list@direction == "horizontal") {
        left_w = calc_before_w(ht_list)
        right_w = calc_after_w(ht_list)
    } else {
        left_w = sum(component_width(ht_list@ht_list[[ which_main_ht(ht_list) ]], 1:4))
        right_w = sum(component_width(ht_list@ht_list[[ which_main_ht(ht_list) ]], 6:9))
    }
    ht_w = unit(1, "npc") - left_w - right_w
    if(include_right) {
        ht_w = unit(1, "npc") - left_w
    }
    ht_w
}

calc_before_h = function(ht_list) {
    i = which_first_ht(ht_list)
    if(i > 1) {
        unit(0, "mm")
    } else {
        sum(component_height(ht_list@ht_list[[ i ]], 6:9))
    }
}

calc_after_h = function(ht_list) {
    i = which_last_ht(ht_list)
    if(i < length(ht_list@ht_list)) {
        unit(0, "mm")
    } else {
        sum(component_height(ht_list@ht_list[[ i ]], 6:9))
    }
}

calc_before_w = function(ht_list) {
    i = which_first_ht(ht_list)
    if(i > 1) {
        unit(0, "mm")
    } else {
        sum(component_width(ht_list@ht_list[[ i ]], 1:4))
    }
}

calc_after_w = function(ht_list) {
    i = which_last_ht(ht_list)
    if(i < length(ht_list@ht_list)) {
        unit(0, "mm")
    } else {
        sum(component_width(ht_list@ht_list[[ i ]], 6:9))
    }
}


guess_align_legend = function(ht_list, 
    heatmap_legend_size, annotation_legend_size,
    heatmap_legend_side, annotation_legend_side,
    test_on) {

    if(test_on == "heatmap_legend") {
        if(heatmap_legend_side == annotation_legend_side) {
            if(!is.null(attr(annotation_legend_size, "multiple"))) {
                if(attr(heatmap_legend_size, "multiple") != 1 || attr(annotation_legend_size, "multiple") != 1) {
                    return("global_center")
                }
            }
        }
        if(attr(heatmap_legend_size, "multiple") != 1) {
            return("global_center")
        }
    }
    if(test_on == "annotation_legend") {
        if(heatmap_legend_side == annotation_legend_side) {
            if(!is.null(attr(heatmap_legend_size, "multiple"))) {
                if(attr(heatmap_legend_size, "multiple") != 1 || attr(annotation_legend_size, "multiple") != 1) {
                    return("global_center")
                }
            }
        }
        if(attr(annotation_legend_size, "multiple") != 1) {
            return("global_center")
        }
    }

    heatmap_legend_size

    align_legend = NULL
    if(heatmap_legend_side == annotation_legend_side) {
        # if the size is less than the heatmap body
        if(ifelse(test_on == "heatmap_legend", heatmap_legend_side, annotation_legend_side) %in% c("left", "right")) {
            ht_h = calc_ht_h(ht_list)
            if(convertHeight(ht_h, "mm", valueOnly = TRUE) >= max(as.numeric(heatmap_legend_size[2]), as.numeric(annotation_legend_size[2]))) {
                align_legend = "heatmap_center"
            }
        } else {
            ht_w = calc_ht_w(ht_list)
            if(convertWidth(ht_w, "mm", valueOnly = TRUE) >= max(as.numeric(heatmap_legend_size[1]), as.numeric(annotation_legend_size[1]))) {
                align_legend = "heatmap_center"
            }
        }
        # if the size if less than excluding top of first heatmap
        if(is.null(align_legend)) {
            if(ifelse(test_on == "heatmap_legend", heatmap_legend_side, annotation_legend_side) %in% c("left", "right")) {
                ht_h = calc_ht_h(ht_list, TRUE)
                if(convertHeight(ht_h, "mm", valueOnly = TRUE) >= max(as.numeric(heatmap_legend_size[2]), as.numeric(annotation_legend_size[2]))) {
                    align_legend = "heatmap_top"
                }
            } else {
                ht_w = calc_ht_w(ht_list, TRUE)
                if(convertWidth(ht_w, "mm", valueOnly = TRUE) >= max(as.numeric(heatmap_legend_size[1]), as.numeric(annotation_legend_size[1]))) {
                    align_legend = "heatmap_left"
                }
            }
        }

        if(is.null(align_legend)) {
            align_legend = "global_center"
        }
    } else {
        # if the size is less than the heatmap body
        if(ifelse(test_on == "heatmap_legend", heatmap_legend_side, annotation_legend_side) %in% c("left", "right")) {
            ht_h = calc_ht_h(ht_list)
            if(convertHeight(ht_h, "mm", valueOnly = TRUE) >= 
                ifelse(test_on == "heatmap_legend", 
                    as.numeric(heatmap_legend_size[2]), as.numeric(annotation_legend_size[2]))) {
                align_legend = "heatmap_center"
            }
        } else {
            ht_w = calc_ht_w(ht_list)
            if(convertWidth(ht_w, "mm", valueOnly = TRUE) >= 
                ifelse(test_on == "heatmap_legend", 
                    as.numeric(heatmap_legend_size[1]), as.numeric(annotation_legend_size[1]))) {
                align_legend = "heatmap_center"
            }
        }
        # if the size if less than excluding top of first heatmap
        if(is.null(align_legend)) {
            if(ifelse(test_on == "heatmap_legend", heatmap_legend_side, annotation_legend_side) %in% c("left", "right")) {
                ht_h = calc_ht_h(ht_list, TRUE)
                if(convertHeight(ht_h, "mm", valueOnly = TRUE) >= 
                    ifelse(test_on == "heatmap_legend", 
                        as.numeric(heatmap_legend_size[2]), as.numeric(annotation_legend_size[2]))) {
                    align_legend = "heatmap_top"
                }
            } else {
                ht_w = calc_ht_w(ht_list, TRUE)
                if(convertWidth(ht_w, "mm", valueOnly = TRUE) >= 
                    ifelse(test_on == "heatmap_legend", 
                        as.numeric(heatmap_legend_size[1]), as.numeric(annotation_legend_size[1]))) {
                    align_legend = "heatmap_left"
                }
            }
        }

        if(is.null(align_legend)) {
            align_legend = "global_center"
        }
    }
    return(align_legend)
}

# == title
# Size of the Heatmap Legends
#
# == param
# -object A `HeatmapList-class` object.
# -legend_list A list of self-defined legend, should be wrapped into `grid::grob` objects.
#     It is normally constructed by `Legend`.
# -... Other arguments.
#
# == detail
# Internally, all heatmap legends are packed by `packLegend` as a single `grid::grob` object.
#
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
    legend_grouping = object@ht_list_param$legend_grouping

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                    }
                }
                if(!is.null(ht@top_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                    }
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(!is.null(ht@bottom_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                    }
                }
                if(!is.null(ht@right_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                    }
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
                }
            }
        } else {
            if(inherits(object@ht_list[[i]], "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                    }
                }
                if(!is.null(ht@top_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                    }
                }
                if(object@ht_list[[i]]@heatmap_param$show_heatmap_legend) {
                    ColorMappingList = c.list(ColorMappingList, object@ht_list[[i]]@matrix_color_mapping)
                    ColorMappingParamList = c.list(ColorMappingParamList, object@ht_list[[i]]@matrix_legend_param)
                }
                if(!is.null(ht@bottom_annotation)) {
                    if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                    }
                }
                if(!is.null(ht@right_annotation)) {
                    if(object@ht_list_param$merge_legends) {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                    }
                }
            } else if(inherits(object@ht_list[[i]], "HeatmapAnnotation")) {
                if(object@ht_list_param$merge_legends || legend_grouping == "adjusted") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(object@ht_list[[i]]))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(object@ht_list[[i]]))
                }
            }
        }
    }

    size = draw_legend(ColorMappingList, ColorMappingParamList, side = side, plot = FALSE, legend_list = legend_list, padding = padding, ...)

    return(size)
})

# == title
# Size of the Annotation Legends
#
# == param
# -object a `HeatmapList-class` object.
# -legend_list A list of self-defined legend, should be wrapped into `grid::grob` objects.
#     It is normally constructed by `Legend`.
# -... Other arguments.
#
# == detail
# Internally, all annotation legends are packed by `packLegend` as a single `grid::grob` object.
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
    legend_grouping = object@ht_list_param$legend_grouping

    ColorMappingList = list()
    ColorMappingParamList = list()
    for(i in seq_along(object@ht_list)) {
        ht = object@ht_list[[i]]
        if(direction == "horizontal") {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                    }
                }
                if(!is.null(ht@top_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                }
                if(!is.null(ht@bottom_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                }
                if(!is.null(ht@right_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                    }
                }
            } else if(inherits(ht, "HeatmapAnnotation")) {
                if(legend_grouping == "original") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht))
                }
            }
        } else {
            if(inherits(ht, "Heatmap")) {
                if(!is.null(ht@left_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@left_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@left_annotation))
                }
                if(!is.null(ht@top_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@top_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@top_annotation))
                    }
                }
                if(!is.null(ht@bottom_annotation)) {
                    if(legend_grouping == "original") {
                        ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@bottom_annotation))
                        ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@bottom_annotation))
                    }
                }
                if(!is.null(ht@right_annotation)) {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht@right_annotation))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht@right_annotation))
                }
            } else if(inherits(ht, "HeatmapAnnotation")) {
                if(legend_grouping == "original") {
                    ColorMappingList = c.list(ColorMappingList, list = get_color_mapping_list(ht))
                    ColorMappingParamList = c.list(ColorMappingParamList, list = get_legend_param_list(ht))
                }
            }
        }
    }

    size = draw_legend(ColorMappingList, ColorMappingParamList, side = side, plot = FALSE, legend_list = legend_list, padding = padding, ...)
    
    return(size)
})

# create a viewport which contains legend
draw_legend = function(ColorMappingList, ColorMappingParamList, side = c("right", "left", "top", "bottom"), 
    plot = TRUE, gap = unit(4, "mm"), legend_list = list(), padding = unit(c(0, 0, 0, 0), "mm"), 
    max_height = unit(par("din")[2], "in"), max_width = unit(par("din")[1], "in"), ...) {

    side = match.arg(side)[1]
    # remove legends which are duplicated by testing the names
    legend_names = sapply(ColorMappingList, function(x) x@name)
    # merge discrete legend if they have the same name
    ColorMappingParamList2 = ColorMappingList2 = vector("list", length(unique(legend_names)))
    names(ColorMappingParamList2) = names(ColorMappingList2) = unique(legend_names)
    for(i in seq_along(legend_names)) {
        if(is.null(ColorMappingList2[[ legend_names[i] ]])) {
            ColorMappingList2[[ legend_names[i] ]] = ColorMappingList[[i]]
            ColorMappingParamList2[[ legend_names[i] ]] = ColorMappingParamList[[i]]
        } else {
            if(ColorMappingList2[[ legend_names[i] ]]@type == "discrete" && ColorMappingList[[i]]@type == "discrete") {
                ColorMappingList2[[ legend_names[i] ]] = c(ColorMappingList2[[ legend_names[i] ]], ColorMappingList[[i]], name = legend_names[i])
            }
        }
    }

    # l = !duplicated(legend_names)
    # ColorMappingList = ColorMappingList[l]
    # ColorMappingParamList = ColorMappingParamList[l]

    n = length(ColorMappingList2)
    if(n == 0 && length(legend_list) == 0) {
        u = unit(c(0, 0), "mm")
        attr(u, "multiple") = 1
        return(u)
    } else {
        cm_grob = c(lapply(seq_along(ColorMappingList2), function(i) color_mapping_legend(ColorMappingList2[[i]], param = ColorMappingParamList2[[i]], plot = FALSE, ...)), legend_list)
        if(side %in% c("left", "right")) {
            pk = packLegend(list = cm_grob, gap = ht_opt$legend_gap[1], direction = "vertical", max_height = max_height)  
        } else {
            pk = packLegend(list = cm_grob, gap = ht_opt$legend_gap[2], direction = "horizontal", max_width = max_width)
        }
    }

    # width = convertWidth(grobWidth(pk), "mm")
    # height = convertHeight(grobHeight(pk), "mm")
    width = width(pk)
    height = height(pk)

    if(plot) {
        if(side == "right") {
            draw(pk, x = padding[2] + width*0.5)
        } else if(side == "left") {
            draw(pk, x = width*0.5)
        } else if(side == "top") {
            draw(pk, y = padding[1] + height*0.5)
        } else if(side == "bottom") {
            draw(pk, y = height*0.5)
        }

        if(pk@multiple > 1) {
            if(is_RStudio_current_dev()) {
                if(ht_opt$message) {
                    message_wrap("It seems you are using RStudio IDE. There are many legends and they are wrapped into multiple rows/columns. The arrangement relies on the physical size of the graphics device. It only generates correct plot in the figure panel, while in the zoomed plot (by clicking the icon 'Zoom') or in the exported plot (by clicking the icon 'Export'), the legend positions might be wrong. You can directly use e.g. pdf() to save the plot into a file.\n\nUse `ht_opt$message = FALSE` to turn off this message.")
                }
            }
        }
    }

    width = width + padding[2] + padding[4]
    height = height + padding[1] + padding[3]
    width = convertWidth(width, "mm")
    height = convertHeight(height, "mm")
    size = unit.c(width, height)

    attr(size, "multiple") = pk@multiple
    return(size)
}
