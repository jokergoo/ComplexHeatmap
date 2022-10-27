
# == title
# Make the Layout of a Single Heatmap
#
# == param
# -object A `Heatmap-class` object.
# 
# == detail
# The layout of the single heatmap will be established by setting the size of each heatmap component.
# Also how to make graphics for heatmap components will be recorded by saving as functions.
#
# Whether to apply row clustering or column clustering affects the layout, so clustering should be applied 
# first by `prepare,Heatmap-method` before making the layout.
#
# This function is only for internal use.
#
# == value
# A `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_layout",
    signature = "Heatmap",
    definition = function(object) {

    if(object@layout$initialized) {
        qqcat("layout is initialized.\n")
        return(object)
    }

    # position of each row-slice
    row_gap = object@matrix_param$row_gap
    column_gap = object@matrix_param$column_gap
    nr_slice = length(object@row_order_list)
    nc_slice = length(object@column_order_list)

    snr = sapply(object@row_order_list, length)
    snc = sapply(object@column_order_list, length)
    if(nr_slice == 1) {
        slice_height = unit(1, "npc")
    } else {
        slice_height = (unit(1, "npc") - sum(row_gap[seq_len(nr_slice-1)]))*(snr/sum(snr))
    }
    for(i in seq_len(nr_slice)) {
        if(i == 1) {
            slice_y = unit(1, "npc")
        } else {
            slice_y = unit.c(slice_y, unit(1, "npc") - sum(slice_height[seq_len(i-1)]) - sum(row_gap[seq_len(i-1)]))
        }
    }

    if(nc_slice == 1) {
        slice_width = unit(1, "npc")
    } else {
        slice_width = (unit(1, "npc") - sum(column_gap[seq_len(nc_slice-1)]))*(snc/sum(snc))
    }
    for(i in seq_len(nc_slice)) {
        if(i == 1) {
            slice_x = unit(0, "npc")
        } else {
            slice_x = unit.c(slice_x, sum(slice_width[seq_len(i-1)]) + sum(column_gap[seq_len(i-1)]))
        }
    }
    object@layout$slice = list(
        x = slice_x, 
        y = slice_y, 
        width = slice_width, 
        height = slice_height,
        just = c("left", "top")
    )

    if(length(object@matrix)) {
        
        ###########################################
        ## heatmap body
        object@layout$layout_index = rbind(heatmapb_body = heatmap_layout_index("heatmap_body"))
        object@layout$graphic_fun_list = list(function(object) {
            for(i in seq_len(nr_slice)) {
                for(j in seq_len(nc_slice)) {
                    draw_heatmap_body(object, kr = i, kc = j, x = slice_x[j], y = slice_y[i], width = slice_width[j], height = slice_height[i], just = c("left", "top"))
                }
            }
        })
    }

    ############################################
    ## title on top or bottom
    column_title = object@column_title
    column_title_side = object@column_title_param$side
    column_title_gp = object@column_title_param$gp
    column_title_rot = object@column_title_param$rot
    if(!is.null(ht_opt$TITLE_PADDING)) {
        title_padding = ht_opt$TITLE_PADDING
    } else {
        title_padding = unit(c(0, 0), "points")
        title_padding[1] = title_padding[1] + unit(5.5, "points") + 
            convertHeight(grobDescent(textGrob(label = "jA", gp = column_title_gp)), "inches")
    }

    if(length(column_title) > 0) {
        if(column_title_side == "top") {
            object@layout$layout_size$column_title_top_height = max_text_height(column_title, gp = column_title_gp, rot = column_title_rot) + sum(title_padding)
            object@layout$layout_index = rbind(object@layout$layout_index, column_title_top = heatmap_layout_index("column_title_top"))
        } else {
            object@layout$layout_size$column_title_bottom_height = max_text_height(column_title, gp = column_title_gp, rot = column_title_rot) + sum(title_padding)
            object@layout$layout_index = rbind(object@layout$layout_index, column_title_bottom = heatmap_layout_index("column_title_bottom"))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            if(length(column_title) == 1 && nc_slice > 1) {
                draw_title(object, k = 1, which = "column")
            } else {
                for(i in seq_len(nc_slice)) {
                    draw_title(object, k = i, which = "column", x = slice_x[i], width = slice_width[i], just = "left")
                }
            }
        })
    }

    ############################################
    ## title on left or right
    row_title = object@row_title
    row_title_side = object@row_title_param$side
    row_title_gp = object@row_title_param$gp
    row_title_rot = object@row_title_param$rot
    if(!is.null(ht_opt$TITLE_PADDING)) {
        title_padding = ht_opt$TITLE_PADDING
    } else {
        title_padding = unit(c(0, 0), "points")
        title_padding[1] = title_padding[1] + unit(5.5, "points") + 
            convertHeight(grobDescent(textGrob(label = "jA", gp = row_title_gp)), "inches")
    }
    if(length(row_title) > 0) {
        if(row_title_side == "left") {
            object@layout$layout_size$row_title_left_width = max_text_width(row_title, gp = row_title_gp, rot = row_title_rot) + sum(title_padding)
            object@layout$layout_index = rbind(object@layout$layout_index, row_title_left = heatmap_layout_index("row_title_left"))
        } else {
            object@layout$layout_size$row_title_right_width = max_text_width(row_title, gp = row_title_gp, rot = row_title_rot) + sum(title_padding)
            object@layout$layout_index = rbind(object@layout$layout_index, row_title_right = heatmap_layout_index("row_title_right"))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            if(length(row_title) == 1 && nr_slice > 1) {
                draw_title(object, k = 1, which = "row")
            } else {
                for(i in seq_len(nr_slice)) {
                    draw_title(object, k = i, which = "row", y = slice_y[i], height = slice_height[i], just = "top")
                }
            }
        })
    }

    ##########################################
    ## dend on left or right
    show_row_dend = object@row_dend_param$show
    row_dend_side = object@row_dend_param$side
    row_dend_width = object@row_dend_param$width
    row_dend_slice = object@row_dend_slice
    cluster_dend_slices = object@row_dend_param$cluster_slices
    if(show_row_dend) {
        if(row_dend_side == "left") {
            object@layout$layout_size$row_dend_left_width = row_dend_width
            object@layout$layout_index = rbind(object@layout$layout_index, row_dend_left = heatmap_layout_index("row_dend_left"))
        } else {
            object@layout$layout_size$row_dend_right_width = row_dend_width
            object@layout$layout_index = rbind(object@layout$layout_index, row_dend_right = heatmap_layout_index("row_dend_right"))
        }
        if(object@row_dend_param$split_by_cutree) {
            row_dend_max_height = dend_heights(row_dend_slice)
        } else {
            row_dend_max_height = dend_heights(row_dend_slice) + max(dend_heights(object@row_dend_list))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            
            if(row_dend_side == "left") {
                pushViewport(viewport(x = unit(0, "npc"), width = unit(1, "npc") - ht_opt$DENDROGRAM_PADDING, just = "left"))
            } else {
                pushViewport(viewport(x = ht_opt$DENDROGRAM_PADDING, width = unit(1, "npc") - ht_opt$DENDROGRAM_PADDING, just = "left"))
            }
            for(i in seq_len(nr_slice)) {
                draw_dend(object, k = i, which = "row", y = slice_y[i], height = slice_height[i], just = "top",
                    max_height = row_dend_max_height)
            }
            if(nr_slice > 1 && cluster_dend_slices) {
                if(row_dend_side == "left") {
                    pushViewport(viewport(xscale = c(0, row_dend_max_height)))
                } else {
                    pushViewport(viewport(xscale = c(0, row_dend_max_height)))
                }
                p = sapply(object@row_dend_list, function(x) {
                    attr(x, "x")/nobs(x)
                })

                nb = sapply(object@row_dend_list, nobs)

                slice_leaf_pos = slice_y
                for(i in seq_len(nr_slice)) {
                    slice_leaf_pos[i] = slice_leaf_pos[i] - slice_height[i]*p[i]
                }
                if(!object@row_dend_param$split_by_cutree) {
                    row_dend_slice = merge_dendrogram(row_dend_slice, object@row_dend_list, only_parent = TRUE)
                }
                row_dend_slice = adjust_dend_by_x(row_dend_slice, slice_leaf_pos)
                grid.dendrogram(row_dend_slice, facing = ifelse(row_dend_side == "left", "right", "left"), gp = object@row_dend_param$gp)
                if(!object@row_dend_param$split_by_cutree && object@heatmap_param$show_parent_dend_line) {
                    dh = dend_heights(object@row_dend_list)
                    if(row_dend_side == "left") {
                        grid.segments(unit(row_dend_max_height - max(dh), "native") - unit(0.5, "mm"), 
                            min(unit.c(slice_leaf_pos[1] + unit(5, "mm"), unit(1, "npc"))), 
                            unit(row_dend_max_height - max(dh), "native") - unit(0.5, "mm"), 
                            max(unit.c(slice_leaf_pos[length(slice_leaf_pos)] - unit(5, "mm"), unit(0, "npc"))),
                            gp = gpar(lty = 3, col = "#666666"))
                    } else {
                        grid.segments(unit(max(dh), "native") + unit(0.5, "mm"), 
                            min(unit.c(slice_leaf_pos[1] + unit(5, "mm"), unit(1, "npc"))), 
                            unit(max(dh), "native") + unit(0.5, "mm"), 
                            max(unit.c(slice_leaf_pos[length(slice_leaf_pos)] - unit(5, "mm"), unit(0, "npc"))),
                            gp = gpar(lty = 3, col = "#666666"))
                    }
                }
                popViewport()
            }
            upViewport()
        })
    }

    ##########################################
    ## dend on top or bottom
    show_column_dend = object@column_dend_param$show
    column_dend_side = object@column_dend_param$side
    column_dend_height = object@column_dend_param$height
    column_dend_slice = object@column_dend_slice
    cluster_column_slices = object@column_dend_param$cluster_slices
    if(show_column_dend) {
        if(column_dend_side == "top") {
            object@layout$layout_size$column_dend_top_height = column_dend_height
            object@layout$layout_index = rbind(object@layout$layout_index, column_dend_top = heatmap_layout_index("column_dend_top"))
        } else {
            object@layout$layout_size$column_dend_bottom_height = column_dend_height
            object@layout$layout_index = rbind(object@layout$layout_index, column_dend_bottom = heatmap_layout_index("column_dend_bottom"))
        }
        if(object@column_dend_param$split_by_cutree) {
            column_dend_max_height = dend_heights(column_dend_slice)
        } else {
            column_dend_max_height = dend_heights(column_dend_slice) + max(dend_heights(object@column_dend_list))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            if(column_dend_side == "top") {
                pushViewport(viewport(y = ht_opt$DENDROGRAM_PADDING, height = unit(1, "npc") - ht_opt$DENDROGRAM_PADDING, just = "bottom"))
            } else {
                pushViewport(viewport(y = unit(0, "npc"), height = unit(1, "npc") - ht_opt$DENDROGRAM_PADDING, just = "bottom"))
            }
            for(i in seq_len(nc_slice)) {
                draw_dend(object, k = i, which = "column", x = slice_x[i], width = slice_width[i], just = "left",
                    max_height = column_dend_max_height)
            }
            if(nc_slice > 1 && cluster_column_slices) {
                if(column_dend_side == "top") {
                    pushViewport(viewport(yscale = c(0, column_dend_max_height)))
                } else {
                    pushViewport(viewport(yscale = c(0, column_dend_max_height)))
                }
                p = sapply(object@column_dend_list, function(x) {
                    attr(x, "x")/nobs(x)
                })

                nb = sapply(object@column_dend_list, nobs)

                slice_leaf_pos = slice_x
                for(i in seq_len(nc_slice)) {
                    slice_leaf_pos[i] = slice_leaf_pos[i] + slice_width[i]*p[i]
                }

                if(!object@column_dend_param$split_by_cutree) {
                    column_dend_slice = merge_dendrogram(column_dend_slice, object@column_dend_list, only_parent = TRUE)
                }
                column_dend_slice = adjust_dend_by_x(column_dend_slice, slice_leaf_pos)
                grid.dendrogram(column_dend_slice, facing = ifelse(column_dend_side == "top", "bottom", "top"), gp = object@column_dend_param$gp)
                if(!object@column_dend_param$split_by_cutree && object@heatmap_param$show_parent_dend_line) {
                    dh = dend_heights(object@column_dend_list)
                    if(row_dend_side == "bottom") {
                        grid.segments(max(unit.c(slice_leaf_pos[1] - unit(5, "mm"), unit(0, "npc"))), 
                            unit(column_dend_max_height - max(dh), "native") - unit(0.5, "mm"), 
                            min(unit.c(slice_leaf_pos[length(slice_leaf_pos)] + unit(5, "mm"), unit(1, "npc"))),
                            unit(column_dend_max_height - max(dh), "native") - unit(0.5, "mm"), 
                            gp = gpar(lty = 3, col = "#666666"))
                    } else {
                        grid.segments(max(unit.c(slice_leaf_pos[1] - unit(5, "mm"), unit(0, "npc"))), 
                            unit(max(dh), "native") + unit(0.5, "mm"), 
                            min(unit.c(slice_leaf_pos[length(slice_leaf_pos)] + unit(5, "mm"), unit(1, "npc"))),
                            unit(max(dh), "native") + unit(0.5, "mm"), 
                            gp = gpar(lty = 3, col = "#666666"))
                    }
                }
                popViewport()
            }
            upViewport()
        })
    }

    #######################################
    ## row_names on left or right
    row_names_side = object@row_names_param$side
    show_row_names = object@row_names_param$show
    row_names_anno = object@row_names_param$anno
    if(show_row_names) {
        row_names_width = row_names_anno@width + ht_opt$DIMNAME_PADDING*2
        row_names_width = min(row_names_width, object@row_names_param$max_width)
        if(row_names_side == "left") {
            object@layout$layout_size$row_names_left_width = row_names_width
            object@layout$layout_index = rbind(object@layout$layout_index, row_names_left = heatmap_layout_index("row_names_left"))
        } else {
            object@layout$layout_size$row_names_right_width = row_names_width
            object@layout$layout_index = rbind(object@layout$layout_index, row_names_right = heatmap_layout_index("row_names_right"))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            for(i in seq_len(nr_slice)) {
                draw_dimnames(object, k = i, which = "row", y = slice_y[i], 
                    height = slice_height[i], width = unit(1, "npc") - ht_opt$DIMNAME_PADDING*2, just = "top")
            }
        })
    }

    #########################################
    ## column_names on top or bottom
    column_names_side = object@column_names_param$side
    show_column_names = object@column_names_param$show
    column_names_anno = object@column_names_param$anno
    if(show_column_names) {
        column_names_height = column_names_anno@height + ht_opt$DIMNAME_PADDING*2
        column_names_height = min(column_names_height, object@column_names_param$max_height)
        if(column_names_side == "top") {
            object@layout$layout_size$column_names_top_height = column_names_height
            object@layout$layout_index = rbind(object@layout$layout_index, column_names_top = heatmap_layout_index("column_names_top"))
        } else {
            object@layout$layout_size$column_names_bottom_height = column_names_height
            object@layout$layout_index = rbind(object@layout$layout_index, column_names_bottom = heatmap_layout_index("column_names_bottom"))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
            for(i in seq_len(nc_slice)) {
                draw_dimnames(object, k = i, which = "column", x = slice_x[i], 
                    width = slice_width[i], height = unit(1, "npc") - ht_opt$DIMNAME_PADDING*2, just = "left")
            }
        })
    }
    
    ##########################################
    ## annotation on top
    annotation = object@top_annotation
    annotation_height = object@top_annotation_param$height
    if(!is.null(annotation)) {
        if(length(annotation@anno_list) > 0) {
            object@layout$layout_size$column_anno_top_height = annotation_height
            object@layout$layout_index = rbind(object@layout$layout_index, column_anno_top = heatmap_layout_index("column_anno_top"))
            
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
                for(i in seq_len(nc_slice)) {
                    draw_annotation(object, k = i, which = "top", x = slice_x[i], width = slice_width[i], 
                        y = ht_opt$COLUMN_ANNO_PADDING, height = unit(1, "npc") - ht_opt$COLUMN_ANNO_PADDING, 
                        just = c("left", "bottom"))
                }
            }) 
        }
    }

    ##########################################
    ## annotation on bottom
    annotation = object@bottom_annotation
    annotation_height = object@bottom_annotation_param$height
    if(!is.null(annotation)) {
        if(length(annotation@anno_list) > 0) {
            object@layout$layout_size$column_anno_bottom_height = annotation_height
            object@layout$layout_index = rbind(object@layout$layout_index, column_anno_bottom = heatmap_layout_index("column_anno_bottom"))
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
                for(i in seq_len(nc_slice)) {
                    draw_annotation(object, k = i, which = "bottom", x = slice_x[i], width = slice_width[i], 
                        y = unit(0, "npc"), height = unit(1, "npc") - ht_opt$COLUMN_ANNO_PADDING, 
                        just = c("left", "bottom"))
                }
            })
        }
    }

    ##########################################
    ## annotation on left
    annotation = object@left_annotation
    annotation_width = object@left_annotation_param$width
    if(!is.null(annotation)) {
        if(length(annotation@anno_list) > 0) {
            object@layout$layout_size$row_anno_left_width = annotation_width
            object@layout$layout_index = rbind(object@layout$layout_index, row_anno_left = heatmap_layout_index("row_anno_left"))
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
                    for(i in seq_len(nr_slice)) {
                        draw_annotation(object, k = i, which = "left",  y = slice_y[i], height = slice_height[i], 
                            x = unit(0, "npc"), width = unit(1, "npc") - ht_opt$ROW_ANNO_PADDING, 
                            just = c("left", "top"))
                    }
                }
            )
        }
    }

    ##########################################
    ## annotation on right
    annotation = object@right_annotation
    annotation_width = object@right_annotation_param$width
    if(!is.null(annotation)) {
        if(length(annotation@anno_list) > 0) {
            object@layout$layout_size$row_anno_right_width = annotation_width
            object@layout$layout_index = rbind(object@layout$layout_index, row_anno_right = heatmap_layout_index("row_anno_right"))
            object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) {
                for(i in seq_len(nr_slice)) {
                    draw_annotation(object, k = i, which = "right", y = slice_y[i], height = slice_height[i], 
                        x = ht_opt$ROW_ANNO_PADDING, width = unit(1, "npc") - ht_opt$ROW_ANNO_PADDING, 
                        just = c("left", "top"))
                }
            })
        }
    }

    layout_size = object@layout$layout_size
    if(is_abs_unit(object@heatmap_param$width)) {
        # recalcualte the width of heatmap body
        object@matrix_param$width = convertWidth(object@heatmap_param$width -
            sum(layout_size$row_title_left_width,
                layout_size$row_dend_left_width,
                layout_size$row_anno_left_width,
                layout_size$row_names_left_width,
                layout_size$row_dend_right_width,
                layout_size$row_anno_right_width,
                layout_size$row_names_right_width,
                layout_size$row_title_right_width), "mm")
        if(unit_to_numeric(object@matrix_param$width[1]) <= 0) {
            stop_wrap("width of the heatmap body is negative, maybe `heatmap_width` you set is too small. Note `heatmap_width` is the width of the complete heatmap.")
        }
    } else if(is_abs_unit(object@matrix_param$width)) {  # e.g. unit(1, "npc")
        object@heatmap_param$width = convertWidth(sum(
            layout_size$row_title_left_width,
            layout_size$row_dend_left_width,
            layout_size$row_names_left_width,
            layout_size$row_dend_right_width,
            layout_size$row_names_right_width,
            layout_size$row_title_right_width,
            layout_size$row_anno_left_width,
            layout_size$row_anno_right_width
        ) + object@matrix_param$width, "mm")
        if(nc_slice > 1) {
            object@heatmap_param$width = object@heatmap_param$width + sum(column_gap[seq_len(nc_slice-1)])
        }
    } else {
        object@heatmap_param$width = unit(1, "npc")
    }

    if(is_abs_unit(object@heatmap_param$height)) {
        object@matrix_param$height = convertHeight(object@heatmap_param$height - 
            sum(layout_size$column_title_top_height,
                layout_size$column_dend_top_height,
                layout_size$column_anno_top_height,
                layout_size$column_names_top_height,
                layout_size$column_title_bottom_height,
                layout_size$column_dend_bottom_height,
                layout_size$column_anno_bottom_height,
                layout_size$column_names_bottom_height), "mm")
        if(unit_to_numeric(object@matrix_param$height[1]) <= 0) {
            stop_wrap("height of the heatmap body is negative, maybe `heatmap_height` you set is too small. Note `heatmap_height` is the height of the complete heatmap.")
        }
    } else if(is_abs_unit(object@matrix_param$height)) {
        object@heatmap_param$height = convertHeight(sum(
            layout_size$column_title_top_height,
            layout_size$column_dend_top_height,
            layout_size$column_anno_top_height,
            layout_size$column_names_top_height,
            layout_size$column_title_bottom_height,
            layout_size$column_dend_bottom_height,
            layout_size$column_anno_bottom_height,
            layout_size$column_names_bottom_height
        ) + object@matrix_param$height, "mm")
        if(nr_slice > 1) {
            object@heatmap_param$height = object@heatmap_param$height + sum(row_gap[seq_len(nr_slice-1)])
        }
    } else {
        object@heatmap_param$height = unit(1, "npc")
    }

    object@heatmap_param$width_is_absolute_unit = is_abs_unit(object@heatmap_param$width) 
    object@heatmap_param$height_is_absolute_unit = is_abs_unit(object@heatmap_param$height) 
    
    object@layout$initialized = TRUE

    return(object)
})

# == title
# Draw the Single Heatmap with Defaults
#
# == param
# -object A `Heatmap-class` object.
#
# == details
# It actually calls `draw,Heatmap-method`, but only with default parameters. If users want to customize the heatmap,
# they can pass parameters directly to `draw,Heatmap-method`.
#
# == value
# The `HeatmapList-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "show",
    signature = "Heatmap",
    definition = function(object) {

    draw(object)
})

# == title
# Add Heatmap to the Heatmap List
#
# == param
# -object A `Heatmap-class` object.
# -x a `Heatmap-class` object, a `HeatmapAnnotation-class` object or a `HeatmapList-class` object.
# -direction Whether the heatmap is added horizontal or vertically?
#
# == details
# Normally we directly use ``+`` for horizontal concatenation and `\%v\%` for vertical concatenation.
#
# == value
# A `HeatmapList-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "add_heatmap",
    signature = "Heatmap",
    definition = function(object, x, direction = c("horizontal", "vertical")) {

    direction = match.arg(direction)[1]

    ht_list = new("HeatmapList")
    ht_list@direction = direction
    
    ht_list = add_heatmap(ht_list, object, direction = direction)
    ht_list = add_heatmap(ht_list, x, direction = direction)
    return(ht_list)

})

# == title
# Widths of Heatmap Components
#
# == param
# -object A `Heatmap-class` object.
# -k Which components in the heatmap. The value should numeric indices or the names
#    of the corresponding row component. See **Detials**.
#
# == details
# All row components are: ``row_title_left``, ``row_dend_left``, ``row_names_left``, ``row_anno_left``,
# ``heatmap_body``, ``row_anno_right``, ``row_names_right``, ``row_dend_right``, ``row_title_right``.
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_width",
    signature = "Heatmap",
    definition = function(object, k = HEATMAP_LAYOUT_ROW_COMPONENT) {

    if(is.numeric(k)) {
        component_name = names(HEATMAP_LAYOUT_ROW_COMPONENT)[k]
    } else {
        component_name = k
    }
    # this function is used for grid.layout, so null unit is allowed
    .single_unit = function(nm) {
        if(nm == "heatmap_body") {
            object@matrix_param$width
        } else {
            object@layout$layout_size[[paste0(nm, "_width")]]
        }
    }
    
    do.call("unit.c", lapply(component_name, .single_unit))
})

# == title
# Heights of Heatmap Components
#
# == param
# -object A `Heatmap-class` object.
# -k Which components in the heatmap. The value should numeric indices or the names
#    of the corresponding column component. See **Detials**.
#
# == detail
# All column components are: ``column_title_top``, ``column_dend_top``, ``column_names_top``, 
# ``column_anno_top``, ``heatmap_body``, ``column_anno_bottom``, ``column_names_bottom``, 
# ``column_dend_bottom``, ``column_title_bottom``.
#
# This function is only for internal use.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_height",
    signature = "Heatmap",
    definition = function(object, k = HEATMAP_LAYOUT_COLUMN_COMPONENT) {

    if(is.numeric(k)) {
        component_name = names(HEATMAP_LAYOUT_COLUMN_COMPONENT)[k]
    } else {
        component_name = k
    }
    # this function is used for grid.layout, so null unit is allowed
    .single_unit = function(nm) {
        if(nm == "heatmap_body") {
            object@matrix_param$height
        } else {
            object@layout$layout_size[[paste0(nm, "_height")]]
        }
    }

    do.call("unit.c", lapply(component_name, .single_unit))
})

has_component = function(object, component) {
    m = object@layout$layout_index
    ind = heatmap_layout_index(component)
    any(m[, 1] == ind[1] & m[, 2] == ind[2])
}


HEATMAP_LAYOUT_COLUMN_COMPONENT = 1:9
names(HEATMAP_LAYOUT_COLUMN_COMPONENT) = c("column_title_top", "column_dend_top", "column_names_top", "column_anno_top",
    "heatmap_body", "column_anno_bottom", "column_names_bottom", "column_dend_bottom", "column_title_bottom")
HEATMAP_LAYOUT_ROW_COMPONENT = 1:9
names(HEATMAP_LAYOUT_ROW_COMPONENT) = c("row_title_left", "row_dend_left", "row_names_left", "row_anno_left",
    "heatmap_body", "row_anno_right", "row_names_right", "row_dend_right", "row_title_right")

heatmap_layout_index = function(nm) {
    if(grepl("column", nm)) {
        ind = c(HEATMAP_LAYOUT_COLUMN_COMPONENT[nm], HEATMAP_LAYOUT_ROW_COMPONENT["heatmap_body"])
    } else if(grepl("row", nm)) {
        ind = c(HEATMAP_LAYOUT_COLUMN_COMPONENT["heatmap_body"], HEATMAP_LAYOUT_ROW_COMPONENT[nm])
    } else if(nm == "heatmap_body") { # heatmap_body
        ind = c(HEATMAP_LAYOUT_COLUMN_COMPONENT["heatmap_body"], HEATMAP_LAYOUT_ROW_COMPONENT["heatmap_body"])
    }
    names(ind) = c("layout.pos.row", "layout.pos.col")
    return(ind)
}

# == title
# Set Width of Heatmap Component
#
# == param
# -object A `Heatmap-class` object.
# -k Which row component? The value should a numeric index or the name
#    of the corresponding row component. See **Detials**.
# -v width of the component, a `grid::unit` object.
#
# == detail
# All row components are: ``row_title_left``, ``row_dend_left``, ``row_names_left``, ``row_anno_left``,
# ``heatmap_body``, ``row_anno_right``, ``row_names_right``, ``row_dend_right``, ``row_title_right``.
#
# This function is only for internal use.
#
# == value
# The `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "set_component_width",
    signature = "Heatmap",
    definition = function(object, k, v) {

    if(is.numeric(k)) {
        nm = names(HEATMAP_LAYOUT_ROW_COMPONENT)[k]
    } else {
        nm = k
    }

    object@layout$layout_size[[ paste0(nm, "_width") ]] = v
    
    if(is_abs_unit(object@matrix_param$width)) {
        object@heatmap_param$width = sum(component_width(object))
    }

    return(object)
})

# == title
# Set Height of Heatmap Component
#
# == param
# -object A `Heatmap-class` object.
# -k Which column component? The value should a numeric index or the name
#    of the corresponding column component. See **Detials**.
# -v Height of the component, a `grid::unit` object.
#
# == detail
# All column components are: ``column_title_top``, ``column_dend_top``, ``column_names_top``, 
# ``column_anno_top``, ``heatmap_body``, ``column_anno_bottom``, ``column_names_bottom``, 
# ``column_dend_bottom``, ``column_title_bottom``.
#
# This function is only for internal use.
#
# == value
# The `Heatmap-class` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "set_component_height",
    signature = "Heatmap",
    definition = function(object, k, v) {

    if(is.numeric(k)) {
        nm = names(HEATMAP_LAYOUT_COLUMN_COMPONENT)[k]
    } else {
        nm = k
    }

    object@layout$layout_size[[ paste0(nm, "_height") ]] = v
    
    if(is_abs_unit(object@matrix_param$height)) {
        object@heatmap_param$height = sum(component_height(object))
    }

    return(object)
})
