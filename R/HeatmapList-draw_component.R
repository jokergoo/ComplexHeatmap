
# == title
# Adjust Heatmap List
#
# == param
# -object A `HeatmapList-class` object.
#
# == details
# This function adjusts settings in all other heatmaps according to the main heatmap.
# It also adjust the size of heatmap annotations to make them aligned nicely.
#
# This function is only for internal use.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "adjust_heatmap_list",
    signature = "HeatmapList",
    definition = function(object) {
    
    verbose = ht_opt("verbose")

    ## this function does mainly two things
    ## 1. calculate viewport/layout for individual heatmaps/annotations
    ## 2. adjust non heatmap body to fill additional space

    ht_gap = object@ht_list_param$ht_gap
    ht_index = which(sapply(object@ht_list, inherits, "Heatmap"))
    n = length(object@ht_list)
    direction = object@direction

    if(direction == "horizontal") {

        # adjust top anntatation, top annotation of all heatmaps should be aligned
        max_top_anno_height = max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, "column_anno_top")))) - ht_opt$COLUMN_ANNO_PADDING
        max_top_anno_height = convertHeight(max_top_anno_height, "mm")
        max_bottom_anno_height = max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, "column_anno_bottom")))) - ht_opt$COLUMN_ANNO_PADDING
        max_bottom_anno_height = convertHeight(max_bottom_anno_height, "mm")
        for(i in ht_index) {
            if(has_component(object@ht_list[[i]], "column_anno_top")) {
                if(verbose) qqcat("adjust height of top annotation of heamtap @{object@ht_list[[i]]@name}\n")
                object@ht_list[[i]]@top_annotation = re_size(object@ht_list[[i]]@top_annotation, height = max_top_anno_height)
                object@ht_list[[i]] = set_component_height(object@ht_list[[i]], "column_anno_top", object@ht_list[[i]]@top_annotation@height + ht_opt$COLUMN_ANNO_PADDING)
            }
            if(has_component(object@ht_list[[i]], "column_anno_bottom")) {   
                if(verbose) qqcat("adjust height of bottom annotation of heamtap @{object@ht_list[[i]]@name}\n")
                object@ht_list[[i]]@bottom_annotation = re_size(object@ht_list[[i]]@bottom_annotation, height = max_bottom_anno_height)
                object@ht_list[[i]] = set_component_height(object@ht_list[[i]], "column_anno_bottom", object@ht_list[[i]]@bottom_annotation@height + ht_opt$COLUMN_ANNO_PADDING)
            }
        }

        # since each heatmap actually has nine rows, calculate the maximum height of corresponding rows in all heatmap 
        max_title_component_width = unit(c(0, 0), "mm")
        max_title_component_height = unit.c(
            max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, "column_title_top")))),
            max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_height(ht, "column_title_bottom"))))
        )
        max_title_component_height = convertHeight(max_title_component_height, "mm")

        max_top_component_height = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                sum(component_height(ht, c("column_dend_top", "column_names_top", "column_anno_top")))
            } else {
                unit(0, "mm")
            }
        })))
        max_top_component_height = convertHeight(max_top_component_height, "mm")
        max_bottom_component_height = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                sum(component_height(ht, c("column_dend_bottom", "column_names_bottom", "column_anno_bottom")))
            } else {
                unit(0, "mm")
            }
        })))
        max_bottom_component_height = convertHeight(max_bottom_component_height, "mm")

        # adjust height 
        if(verbose) qqcat("adjust title/dend height of all heatmaps\n")
        for(i in ht_index) {
            object@ht_list[[i]] = set_component_height(object@ht_list[[i]], "column_title_top", max_title_component_height[1])
            object@ht_list[[i]] = set_component_height(object@ht_list[[i]], "column_title_bottom", max_title_component_height[2])

            ht = object@ht_list[[i]]
            object@ht_list[[i]] = set_component_height(object@ht_list[[i]], "column_dend_top", max_top_component_height - sum(component_height(object@ht_list[[i]], k = c("column_names_top", "column_anno_top"))))
            object@ht_list[[i]] = set_component_height(object@ht_list[[i]], "column_dend_bottom", max_bottom_component_height - sum(component_height(object@ht_list[[i]], k = c("column_names_bottom", "column_anno_bottom"))))
        }

        if(verbose) qqcat("adjust height to the main heatmap\n")
        i_main = object@ht_list_param$main_heatmap
        for(i in ht_index) {
            if(i != i_main) {
                object@ht_list[[i]]@matrix_param$height = object@ht_list[[i_main]]@matrix_param$height
                object@ht_list[[i]]@heatmap_param$height = object@ht_list[[i_main]]@heatmap_param$height
            }
        }

        ## only some of the heatmaps have null unit and the number is the number of columns
        total_fixed_width = unit(0, "mm")
        total_null_units_lt = list()
        for(i in seq_along(object@ht_list)) {
            ht = object@ht_list[[i]]
            if(inherits(ht, "Heatmap")) {
                total_fixed_width = total_fixed_width + sum(component_width(ht, setdiff(names(HEATMAP_LAYOUT_ROW_COMPONENT), "heatmap_body")))
                if(is_abs_unit(ht@matrix_param$width)) {
                    total_fixed_width = total_fixed_width + ht@matrix_param$width
                } else {
                    total_null_units_lt = c(total_null_units_lt, list(ht@matrix_param$width))
                }
            } else {
                total_fixed_width = total_fixed_width + size(ht)
            }
        }
        if(n > 1) {
            total_fixed_width = total_fixed_width + sum(ht_gap[seq_len(n-1)])
        }
        if(!all(sapply(total_null_units_lt, is.unit))) {
            warning_wrap("Since some of the heatmap_body_width is numeric, all heatmaps should explicitly specify heatmap_body_width as null units or numeric, or else the numeric width is treated as number of columns and will be normalized to other heatmaps.")
        }
        total_null_units = sum(unlist(total_null_units_lt))
        if(total_null_units == 0) {
            heatmap_width = lapply(object@ht_list, function(ht) {
                if(inherits(ht, "Heatmap")) {
                    ht@heatmap_param$width
                } else {
                    size(ht)
                }
            })
        } else {
            unit_per_null = 1/total_null_units*(unit(1, "npc") - convertWidth(total_fixed_width, "mm"))

            heatmap_width = lapply(object@ht_list, function(ht) {
                if(inherits(ht, "Heatmap")) {
                    if(is_abs_unit(ht@matrix_param$width)) {
                        ht@heatmap_param$width
                    } else {
                        sum(component_width(ht, setdiff(names(HEATMAP_LAYOUT_ROW_COMPONENT), "heatmap_body"))) + unit_to_numeric(ht@matrix_param$width[1])*unit_per_null
                    }
                } else {
                    size(ht)
                }
            })
        }

        heatmap_width = do.call("unit.c", heatmap_width)

        object@layout$heatmap_width = heatmap_width
        object@layout$max_top_component_height = max_top_component_height
        object@layout$max_bottom_component_height = max_bottom_component_height

        # and calcualte proper paddings
        if(TRUE) {
            row_anno_max_top_extended = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
                if(inherits(ht, "HeatmapAnnotation")) {
                    ht@extended[3]
                } else if(inherits(ht, "Heatmap")) {
                    u = unit(0, "mm")
                    if(!is.null(ht@left_annotation)) {
                        u = unit.c(u, ht@left_annotation@extended[3])
                    }
                    if(!is.null(ht@right_annotation)) {
                        u = unit.c(u, ht@right_annotation@extended[3])
                    }
                    u
                } else {
                    unit(0, "mm")
                }
            })))
            row_anno_max_top_extended = convertHeight(row_anno_max_top_extended, "mm")

            row_anno_max_bottom_extended = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
                if(inherits(ht, "HeatmapAnnotation")) {
                    ht@extended[1]
                } else if(inherits(ht, "Heatmap")) {
                    u = unit(0, "mm")
                    if(!is.null(ht@left_annotation)) {
                        u = unit.c(u, ht@left_annotation@extended[1])
                    }
                    if(!is.null(ht@right_annotation)) {
                        u = unit.c(u, ht@right_annotation@extended[1])
                    }
                    u
                } else {
                    unit(0, "mm")
                }
            })))
            row_anno_max_bottom_extended = convertHeight(row_anno_max_bottom_extended, "mm")
            
            ## save row_anno_max_top_extended and max_top_component_height and adjust padding when the layout is completely initialized
            object@layout$row_anno_max_top_extended = row_anno_max_top_extended
            object@layout$max_top_component_height = max_top_component_height
            object@layout$row_anno_max_bottom_extended = row_anno_max_bottom_extended
            object@layout$max_bottom_component_height = max_bottom_component_height
            object@layout$max_title_component_height = max_title_component_height
            object@layout$max_title_component_width = max_title_component_width

            ## left and right
            column_anno_max_left_extended = unit(0, "mm")
            max_left_component_width = unit(0, "mm")
            if(inherits(object@ht_list[[1]], "Heatmap")) {
                ht_first = object@ht_list[[1]]
                max_left_component_width = sum(component_width(ht_first, c("row_names_left", "row_dend_left", "row_anno_left", "row_title_left")))
                u = unit(0, "mm")
                if(!is.null(ht_first@top_annotation)) {
                    u = unit.c(u, ht_first@top_annotation@extended[2])
                }
                if(!is.null(ht_first@bottom_annotation)) {
                    u = unit.c(u, ht_first@bottom_annotation@extended[2])
                }
                column_anno_max_left_extended = max(u)
                max_left_component_width = convertWidth(max_left_component_width, "mm")
                column_anno_max_left_extended = convertWidth(column_anno_max_left_extended, "mm")
            }

            column_anno_max_right_extended = unit(0, "mm")
            max_right_component_width = unit(0, "mm")
            if(inherits(object@ht_list[[ length(object@ht_list) ]], "Heatmap")) {
                ht_last = object@ht_list[[ length(object@ht_list) ]]
                max_right_component_width = sum(component_width(ht_last, c("row_names_right", "row_dend_right", "row_anno_right", "row_title_right")))
                u = unit(0, "mm")
                if(!is.null(ht_last@top_annotation)) {
                    u = unit.c(u, ht_last@top_annotation@extended[4])
                }
                if(!is.null(ht_last@bottom_annotation)) {
                    u = unit.c(u, ht_last@bottom_annotation@extended[4])
                }
                column_anno_max_right_extended = max(u)
                max_right_component_width = convertWidth(max_right_component_width, "mm")
                column_anno_max_right_extended = convertWidth(column_anno_max_right_extended, "mm")
            }

            object@layout$column_anno_max_left_extended = column_anno_max_left_extended
            object@layout$max_left_component_width = max_left_component_width
            object@layout$column_anno_max_right_extended = column_anno_max_right_extended
            object@layout$max_right_component_width = max_right_component_width
        }
    } else {
        # adjust left anntatation, right annotation of all heatmaps should be aligned
        max_left_anno_width = max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_width(ht, "row_anno_left")))) - ht_opt$ROW_ANNO_PADDING
        max_left_anno_width = convertWidth(max_left_anno_width, "mm")
        max_right_anno_width = max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_width(ht, "row_anno_right")))) - ht_opt$ROW_ANNO_PADDING
        max_right_anno_width = convertWidth(max_right_anno_width, "mm")
        for(i in ht_index) {
            if(has_component(object@ht_list[[i]], "row_anno_left")) {
                if(verbose) qqcat("adjust width of left annotation of heamtap @{object@ht_list[[i]]@name}\n")
                object@ht_list[[i]]@left_annotation = re_size(object@ht_list[[i]]@left_annotation, width = max_left_anno_width)
                object@ht_list[[i]] = set_component_width(object@ht_list[[i]], "row_anno_left", object@ht_list[[i]]@left_annotation@width + ht_opt$ROW_ANNO_PADDING)
            }
            if(has_component(object@ht_list[[i]], "row_anno_right")) {   
                if(verbose) qqcat("adjust width of right annotation of heamtap @{object@ht_list[[i]]@name}\n")
                object@ht_list[[i]]@right_annotation = re_size(object@ht_list[[i]]@right_annotation, width = max_right_anno_width)
                object@ht_list[[i]] = set_component_width(object@ht_list[[i]], "row_anno_right", object@ht_list[[i]]@right_annotation@width + ht_opt$ROW_ANNO_PADDING)
            }
        }

        max_title_component_height = unit(c(0, 0), "mm")
        max_title_component_width = unit.c(
            max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_width(ht, "row_title_left")))),
            max(do.call("unit.c", lapply(object@ht_list[ht_index], function(ht) component_width(ht, "row_title_right"))))
        )
        max_title_component_width = convertWidth(max_title_component_width, "mm")

        max_left_component_width = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                sum(component_width(ht, c("row_dend_left", "row_names_left", "row_anno_left")))   
            } else {
                unit(0, "mm")
            }
        })))
        max_left_component_width = convertWidth(max_left_component_width, "mm")
        max_right_component_width = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                sum(component_width(ht, c("row_dend_right", "row_names_right", "row_anno_right")))
            } else {
                unit(0, "mm")
            }
        })))
        max_right_component_width = convertWidth(max_right_component_width, "mm")

        # adjust width 
        if(verbose) qqcat("adjust title/dend width of all heatmaps\n")
        for(i in ht_index) {
            object@ht_list[[i]] = set_component_width(object@ht_list[[i]], "row_title_left", max_title_component_width[1])
            object@ht_list[[i]] = set_component_width(object@ht_list[[i]], "row_title_right", max_title_component_width[2])

            ht = object@ht_list[[i]]
            object@ht_list[[i]] = set_component_width(object@ht_list[[i]], "row_dend_left", max_left_component_width - sum(component_width(object@ht_list[[i]], k = c("row_names_left", "row_anno_left"))))
            object@ht_list[[i]] = set_component_width(object@ht_list[[i]], "row_dend_right", max_right_component_width - sum(component_width(object@ht_list[[i]], k = c("row_names_right", "row_anno_right"))))
        }

        if(verbose) qqcat("adjust width to the main heatmap\n")
        i_main = object@ht_list_param$main_heatmap
        for(i in ht_index) {
            if(i != i_main) {
                object@ht_list[[i]]@matrix_param$width = object@ht_list[[i_main]]@matrix_param$width
                object@ht_list[[i]]@heatmap_param$width = object@ht_list[[i_main]]@heatmap_param$width
            }
        }

        ## only some of the heatmaps have null unit and the number is the number of rows
        total_fixed_height = unit(0, "mm")
        total_null_units_lt = list()
        for(i in seq_along(object@ht_list)) {
            ht = object@ht_list[[i]]
            if(inherits(ht, "Heatmap")) {
                total_fixed_height = total_fixed_height + sum(component_height(ht, setdiff(names(HEATMAP_LAYOUT_COLUMN_COMPONENT), "heatmap_body")))
                if(is_abs_unit(ht@matrix_param$height)) {
                    total_fixed_height = total_fixed_height + ht@matrix_param$height
                } else {
                    total_null_units_lt = c(total_null_units_lt, list(ht@matrix_param$height))
                }
            } else {
                total_fixed_height = total_fixed_height + size(ht)
            }
        }
        if(n > 1) {
            total_fixed_height = total_fixed_height + sum(ht_gap[seq_len(n-1)])
        }
        if(!all(sapply(total_null_units_lt, is.unit))) {
            warning_wrap("Since some of the heatmap_body_height is numeric, all heatmaps should explicitly specify heatmap_body_height as null units or numeric, or else the numeric height is treated as number of rows and will be normalized to other heatmaps.")
        }
        total_null_units = sum(unlist(total_null_units_lt))
        if(total_null_units == 0) {
            heatmap_height = lapply(object@ht_list, function(ht) {
                if(inherits(ht, "Heatmap")) {
                    ht@heatmap_param$height
                } else {
                    size(ht)
                }
            })
        } else {
            unit_per_null = 1/total_null_units*(unit(1, "npc") - convertHeight(total_fixed_height, "mm"))

            heatmap_height = lapply(object@ht_list, function(ht) {
                if(inherits(ht, "Heatmap")) {
                    if(is_abs_unit(ht@matrix_param$height)) {
                        ht@heatmap_param$height
                    } else {
                        sum(component_height(ht, setdiff(names(HEATMAP_LAYOUT_COLUMN_COMPONENT), "heatmap_body"))) + unit_to_numeric(ht@matrix_param$height[1])*unit_per_null
                    }
                } else {
                    size(ht)
                }
            })
        }

        heatmap_height = do.call("unit.c", heatmap_height)

        object@layout$heatmap_height = heatmap_height
        object@layout$max_left_component_width = max_left_component_width
        object@layout$max_right_component_width = max_right_component_width
        
        # and calcualte proper paddings
        if(TRUE) {
            column_anno_max_left_extended = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
                if(inherits(ht, "HeatmapAnnotation")) {
                    ht@extended[2]
                } else if(inherits(ht, "Heatmap")) {
                    u = unit(0, "mm")
                    if(!is.null(ht@top_annotation)) {
                        u = unit.c(u, ht@top_annotation@extended[2])
                    }
                    if(!is.null(ht@bottom_annotation)) {
                        u = unit.c(u, ht@bottom_annotation@extended[2])
                    }
                    u
                } else {
                    unit(0, "mm")
                }
            })))
            column_anno_max_left_extended = convertHeight(column_anno_max_left_extended, "mm")

            column_anno_max_right_extended = max(do.call("unit.c", lapply(object@ht_list, function(ht) {
                if(inherits(ht, "HeatmapAnnotation")) {
                    ht@extended[4]
                } else if(inherits(ht, "Heatmap")) {
                    u = unit(0, "mm")
                    if(!is.null(ht@top_annotation)) {
                        u = unit.c(u, ht@top_annotation@extended[4])
                    }
                    if(!is.null(ht@top_annotation)) {
                        u = unit.c(u, ht@top_annotation@extended[4])
                    }
                    u
                } else {
                    unit(0, "mm")
                }
            })))
            column_anno_max_right_extended = convertHeight(column_anno_max_right_extended, "mm")
            
            ## save row_anno_max_top_extended and max_top_component_height and adjust padding when the layout is completely initialized
            object@layout$column_anno_max_left_extended = column_anno_max_left_extended
            object@layout$max_left_component_width = max_left_component_width
            object@layout$column_anno_max_right_extended = column_anno_max_right_extended
            object@layout$max_right_component_width = max_right_component_width
            object@layout$max_title_component_width = max_title_component_width
            object@layout$max_title_component_height = max_title_component_height

            ## top and bottom
            row_anno_max_top_extended = unit(0, "mm")
            max_top_component_height = unit(0, "mm")
            if(inherits(object@ht_list[[1]], "Heatmap")) {
                ht_first = object@ht_list[[1]]
                max_top_component_height = sum(component_height(ht_first, c("column_names_top", "column_dend_top", "column_anno_top", "column_title_top")))

                u = unit(0, "mm")
                if(!is.null(ht_first@left_annotation)) {
                    u = unit.c(u, ht_first@left_annotation@extended[3])
                }
                if(!is.null(ht_first@right_annotation)) {
                    u = unit.c(u, ht_first@right_annotation@extended[3])
                }
                row_anno_max_top_extended = max(u)
                max_top_component_height = convertHeight(max_top_component_height, "mm")
                row_anno_max_top_extended = convertHeight(row_anno_max_top_extended, "mm")
            }

            row_anno_max_bottom_extended = unit(0, "mm")
            max_bottom_component_height = unit(0, "mm")
            if(inherits(object@ht_list[[ length(object@ht_list) ]], "Heatmap")) {
                ht_last = object@ht_list[[ length(object@ht_list) ]]
                max_bottom_component_height = sum(component_height(ht_last, c("column_names_bottom", "column_dend_bottom", "column_anno_bottom", "column_title_bottom")))
                u = unit(0, "mm")
                if(!is.null(ht_last@left_annotation)) {
                    u = unit.c(u, ht_last@left_annotation@extended[1])
                }
                if(!is.null(ht_last@right_annotation)) {
                    u = unit.c(u, ht_last@right_annotation@extended[1])
                }
                row_anno_max_bottom_extended = max(u)
                max_bottom_component_height = convertHeight(max_bottom_component_height, "mm")
                row_anno_max_bottom_extended = convertHeight(row_anno_max_bottom_extended, "mm")
            }

            object@layout$row_anno_max_top_extended = row_anno_max_top_extended
            object@layout$max_top_component_height = max_top_component_height
            object@layout$row_anno_max_bottom_extended = row_anno_max_bottom_extended
            object@layout$max_bottom_component_height = max_bottom_component_height
        }
    }

    adjust_annotation_extension = object@ht_list_param$adjust_annotation_extension

    # the padding of the heatmap list should be recorded because if the total wdith of e.g. heatmap body
    # is a fixed value, the width should added by the padding
    padding = unit(c(0, 0, 0, 0), "mm")
    if(is.null(adjust_annotation_extension)) {
        if(direction == "horizontal") {
            # right side
            if(inherits(object@ht_list[[n]], "Heatmap")) {
                # if the last heatmap has nothing on the right while something on the right of heatmap list
                if(!(has_component(object@ht_list[[n]], "row_anno_right") ||
                     has_component(object@ht_list[[n]], "row_names_right") ||
                     has_component(object@ht_list[[n]], "row_dend_right") ||
                     has_component(object@ht_list[[n]], "row_title_right"))) {
                    if(has_heatmap_list_component(object, "row_title_right") || 
                       has_heatmap_list_component(object, "heatmap_legend_right") || 
                       has_heatmap_list_component(object, "annotation_legend_right")) {
                        object@layout$column_anno_max_right_extended = unit(0, "mm")
                        adjust_annotation_extension = TRUE
                    }
                }
            }
            # left side
            if(inherits(object@ht_list[[1]], "Heatmap")) {
                if(!(has_component(object@ht_list[[1]], "row_anno_left") ||
                     has_component(object@ht_list[[1]], "row_names_left") ||
                     has_component(object@ht_list[[1]], "row_dend_left") ||
                     has_component(object@ht_list[[1]], "row_title_left"))) {
                    if(has_heatmap_list_component(object, "row_title_left") || 
                       has_heatmap_list_component(object, "heatmap_legend_left") || 
                       has_heatmap_list_component(object, "annotation_legend_left")) {
                        object@layout$column_anno_max_left_extended = unit(0, "mm")
                        adjust_annotation_extension = TRUE
                    }
                }
            }
            # bottom side
            if(has_heatmap_list_component(object, "column_title_bottom") || 
               has_heatmap_list_component(object, "heatmap_legend_bottom") || 
               has_heatmap_list_component(object, "annotation_legend_bottom")) {
                object@layout$row_anno_max_bottom_extended = unit(0, "mm")
                adjust_annotation_extension = TRUE
            }

            if(has_heatmap_list_component(object, "column_title_top") || 
               has_heatmap_list_component(object, "heatmap_legend_top") || 
               has_heatmap_list_component(object, "annotation_legend_top")) {
                object@layout$row_anno_max_top_extended = unit(0, "mm")
                adjust_annotation_extension = TRUE
            }

        } else {
            if(inherits(object@ht_list[[n]], "Heatmap")) {
                # if the last heatmap has nothing on the right while something on the right of heatmap list
                if(!(has_component(object@ht_list[[n]], "column_anno_bottom") ||
                     has_component(object@ht_list[[n]], "column_names_bottom") ||
                     has_component(object@ht_list[[n]], "column_dend_bottom") ||
                     has_component(object@ht_list[[n]], "column_title_bottom"))) {
                    if(has_heatmap_list_component(object, "column_title_bottom") || 
                       has_heatmap_list_component(object, "heatmap_legend_bottom") || 
                       has_heatmap_list_component(object, "annotation_legend_bottom")) {
                        object@layout$row_anno_max_bottom_extended = unit(0, "mm")
                        adjust_annotation_extension = TRUE
                    }
                }
            }
            if(inherits(object@ht_list[[1]], "Heatmap")) {
                if(!(has_component(object@ht_list[[1]], "column_anno_top") ||
                     has_component(object@ht_list[[1]], "column_names_top") ||
                     has_component(object@ht_list[[1]], "column_dend_top") ||
                     has_component(object@ht_list[[1]], "column_title_top"))) {
                    if(has_heatmap_list_component(object, "column_title_top") || 
                       has_heatmap_list_component(object, "heatmap_legend_top") || 
                       has_heatmap_list_component(object, "annotation_legend_top")) {
                        object@layout$row_anno_max_top_extended = unit(0, "mm")
                        adjust_annotation_extension = TRUE
                    }
                }
            }

            if(has_heatmap_list_component(object, "column_title_bottom") || 
               has_heatmap_list_component(object, "heatmap_legend_bottom") || 
               has_heatmap_list_component(object, "annotation_legend_bottom")) {
                object@layout$row_anno_max_bottom_extended = unit(0, "mm")
                adjust_annotation_extension = TRUE
            }

            if(has_heatmap_list_component(object, "column_title_top") || 
               has_heatmap_list_component(object, "heatmap_legend_top") || 
               has_heatmap_list_component(object, "annotation_legend_top")) {
                object@layout$row_anno_max_top_extended = unit(0, "mm")
                adjust_annotation_extension = TRUE
            }
        }
    }

    if(is.null(adjust_annotation_extension)) adjust_annotation_extension = TRUE
    if(adjust_annotation_extension) {
        # note e.g. max_*_component_height does not include the height of titles
        if(unit_to_numeric(object@layout$row_anno_max_bottom_extended[1]) > unit_to_numeric(object@layout$max_bottom_component_height[1]) + unit_to_numeric(object@layout$max_title_component_height[2])) {
            padding[1] = object@layout$row_anno_max_bottom_extended - object@layout$max_bottom_component_height - object@layout$max_title_component_height[2]
        }
        if(unit_to_numeric(object@layout$column_anno_max_left_extended[1]) > unit_to_numeric(object@layout$max_left_component_width[1]) + unit_to_numeric(object@layout$max_title_component_width[1])) {
            padding[2] = object@layout$column_anno_max_left_extended - object@layout$max_left_component_width - object@layout$max_title_component_width[1]
        }
            
        if(unit_to_numeric(object@layout$row_anno_max_top_extended[1]) > unit_to_numeric(object@layout$max_top_component_height[1]) + unit_to_numeric(object@layout$max_title_component_height[1])) {
            padding[3] = object@layout$row_anno_max_top_extended - object@layout$max_top_component_height - object@layout$max_title_component_height[1]
        }
        if(unit_to_numeric(object@layout$column_anno_max_right_extended[1]) > unit_to_numeric(object@layout$max_right_component_width[1]) + unit_to_numeric(object@layout$max_title_component_width[2])) {
            padding[4] = object@layout$column_anno_max_right_extended - object@layout$max_right_component_width - object@layout$max_title_component_width[2]
        }
    }
    object@layout$heatmap_list_padding = padding

    return(object)
})

# == title
# Draw the List of Heatmaps
#
# == param
# -object A `HeatmapList-class` object.
#
# == details
# It only draws the list of heatmaps without legends and titles.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_list",
    signature = "HeatmapList",
    definition = function(object) {

    n = length(object@ht_list)
    ht_gap = object@ht_list_param$ht_gap

    padding = object@layout$heatmap_list_padding

    pushViewport(viewport(x = padding[2], y = padding[1], width = unit(1, "npc") - padding[2] - padding[4],
        height = unit(1, "npc") - padding[1] - padding[3], just = c("left", "bottom")))

    if(object@direction == "horizontal") {

        heatmap_width = object@layout$heatmap_width
        max_bottom_component_height = object@layout$max_bottom_component_height + object@layout$max_title_component_height[2]
        max_top_component_height = object@layout$max_top_component_height + object@layout$max_title_component_height[1]

        if(all(sapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                ncol(ht@matrix) == 0
            } else {
                TRUE
            }
        }))) {
            if(has_heatmap_list_component(object, "annotation_legend_top") || 
               has_heatmap_list_component(object, "heatmap_legend_top")) {
                # max_top_component_height = object@layout$row_anno_max_top_extended
                max_top_component_height = unit(0, "mm")
            }
            if(has_heatmap_list_component(object, "annotation_legend_bottom") || 
               has_heatmap_list_component(object, "heatmap_legend_bottom")) {
                # max_bottom_component_height = object@layout$row_anno_max_bottom_extended
                max_bottom_component_height = unit(0, "mm")
            }
        }

        pushViewport(viewport(name = "main_heatmap_list"))

        i_main = object@ht_list_param$main_heatmap
        ht_main = object@ht_list[[i_main]]
        slice_y = ht_main@layout$slice$y
        n_slice = length(slice_y)
        slice_height = ht_main@layout$slice$height
        slice_just = ht_main@layout$slice$just

        for(i in seq_len(n)) {
            ht = object@ht_list[[i]]

            if(i > 1) {
                x = sum(heatmap_width[seq_len(i-1)]) + sum(ht_gap[seq_len(i-1)])
            } else {
                x = unit(0, "npc")
            }

            pushViewport(viewport(x = x, y = unit(0, "npc"), width = heatmap_width[i], just = c("left", "bottom"), name = paste0("heatmap_", object@ht_list[[i]]@name)))
            if(inherits(ht, "Heatmap")) {
                draw(ht, internal = TRUE)
            } else if(inherits(ht, "HeatmapAnnotation")) {
                # if the HeatmapAnnotation contains anno_mark() and it is split into more than one slices
                anno_mark_param = list()
                if(n_slice > 1) {
                    all_anno_type = anno_type(ht)
                    if(any(c("anno_zoom", "anno_mark") %in% all_anno_type)) {
                        ## only make the anno_mark annotation
                        pushViewport(viewport(y = max_bottom_component_height, height = unit(1, "npc") - max_top_component_height - max_bottom_component_height, just = c("bottom")))
                        ro_lt = ht_main@row_order_list
                        # calcualte the position of each row with taking "gaps" into account
                        .scale = c(0, 1)

                        .pos = NULL
                        for(i in seq_along(ro_lt)) {
                            # assume slices are align to top `slice_just` contains "top"
                            .pos1 = slice_y[i] - (seq_along(ro_lt[[i]]) - 0.5)/length(ro_lt[[i]]) * slice_height[i]
                            .pos1 = convertY(.pos1, "native", valueOnly = TRUE)
                            .pos = c(.pos, .pos1)
                        }

                        anno_mark_param$.scale = .scale
                        anno_mark_param$.pos = .pos
                        anno_mark_param$index = unlist(ro_lt)
                        
                        anno_mark_param$vp_height = convertHeight(unit(1, "npc"), "cm")
                        anno_mark_param$vp_width = unit(1, "npc")
                        anno_mark_param$vp_just = "top"
                        anno_mark_param$vp_x = unit(0.5, "npc")
                        anno_mark_param$vp_y = unit(1, "npc")
                        popViewport()
                    }
                }

                # calcualte the position of the heatmap body
                pushViewport(viewport(y = max_bottom_component_height, height = unit(1, "npc") - max_top_component_height - max_bottom_component_height, just = c("bottom")))
                for(j in seq_len(n_slice)) {
                    draw(ht, index = ht_main@row_order_list[[j]], y = slice_y[j], height = slice_height[j], just = slice_just[2], k = j, n = n_slice, anno_mark_param = anno_mark_param)
                }
                upViewport()
            }
            upViewport()
        }
        upViewport()
    } else {
        heatmap_height = object@layout$heatmap_height
        max_left_component_width = object@layout$max_left_component_width + object@layout$max_title_component_width[1]
        max_right_component_width = object@layout$max_right_component_width + object@layout$max_title_component_width[2]

        if(all(sapply(object@ht_list, function(ht) {
            if(inherits(ht, "Heatmap")) {
                nrow(ht@matrix) == 0
            } else {
                TRUE
            }
        }))) {
            if(has_heatmap_list_component(object, "annotation_legend_right") || 
               has_heatmap_list_component(object, "heatmap_legend_right")) {
                # max_right_component_width = object@layout$column_anno_max_right_extended
                max_right_component_width = unit(0, "mm") 
            }
            if(has_heatmap_list_component(object, "annotation_legend_left") || 
               has_heatmap_list_component(object, "heatmap_legend_left")) {
                # max_left_component_width = object@layout$column_anno_max_left_extended
                max_left_component_width = unit(0, "mm")
            }
        }

        pushViewport(viewport(name = "main_heatmap_list"))

        i_main = object@ht_list_param$main_heatmap
        ht_main = object@ht_list[[i_main]]
        slice_x = ht_main@layout$slice$x
        n_slice = length(slice_x)
        slice_width = ht_main@layout$slice$width
        slice_just = ht_main@layout$slice$just

        for(i in seq_len(n)) {
            ht = object@ht_list[[i]]

            if(i == 1) {
                y = unit(1, "npc")
            } else {
                y = unit(1, "npc") - sum(heatmap_height[seq_len(i-1)]) - sum(ht_gap[seq_len(i-1)])
            }
            
            pushViewport(viewport(y = y, x = unit(0, "npc"), height = heatmap_height[i], just = c("left", "top"), name = paste0("heatmap_", object@ht_list[[i]]@name)))
            if(inherits(ht, "Heatmap")) {
                draw(ht, internal = TRUE)
            } else if(inherits(ht, "HeatmapAnnotation")) {
                # if the HeatmapAnnotation contains anno_mark() and it is split into more than one slices
                anno_mark_param = list()
                if(n_slice > 1) {
                    all_anno_type = anno_type(ht)
                    if(any(c("anno_zoom", "anno_mark") %in% all_anno_type)) {
                        ## only make the anno_mark annotation
                        pushViewport(viewport(x = max_left_component_width, width = unit(1, "npc") - max_left_component_width - max_right_component_width, just = c("left")))
                        co_lt = ht_main@column_order_list
                        .scale = c(0, 1)

                        .pos = NULL
                        for(i in seq_along(co_lt)) {
                            # assume slices are align to left, `slice_just` contains "left"
                            .pos1 = slice_x[i] + (seq_along(co_lt[[i]]) - 0.5)/length(co_lt[[i]]) * slice_width[i]
                            .pos1 = convertX(.pos1, "native", valueOnly = TRUE)
                            .pos = c(.pos, .pos1)
                        }

                        anno_mark_param$.scale = .scale
                        anno_mark_param$.pos = .pos
                        anno_mark_param$index = unlist(co_lt)
                        
                        anno_mark_param$vp_height = unit(1, "npc")
                        anno_mark_param$vp_width = convertWidth(unit(1, "npc"), "cm")
                        anno_mark_param$vp_just = "left"
                        anno_mark_param$vp_x = unit(0, "npc")
                        anno_mark_param$vp_y = unit(0.5, "npc")
                        popViewport()
                    }
                }

                # calcualte the position of the heatmap body
                pushViewport(viewport(x = max_left_component_width, width = unit(1, "npc") - max_left_component_width - max_right_component_width, just = c("left")))
                for(j in seq_len(n_slice)) {
                    draw(ht, index = ht_main@column_order_list[[j]], x = slice_x[j], width = slice_width[j], just = slice_just[1], k = j, n = n_slice, anno_mark_param = anno_mark_param)
                }
                upViewport()
            }
            upViewport()
        }

        upViewport()
    }
    upViewport()

})

# == title
# Draw Heatmap List Title
#
# == param
# -object A `HeatmapList-class` object.
# -which Is it a row title or a column title.
#
# == details
# A viewport is created which contains heatmap list title.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_title",
    signature = "HeatmapList",
    definition = function(object,
    which = c("column", "row")) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_title_param$side,
        "column" = object@column_title_param$side)

    gp = switch(which,
        "row" = object@row_title_param$gp,
        "column" = object@column_title_param$gp)

    title = switch(which,
        "row" = object@row_title,
        "column" = object@column_title)

    if(!is.null(ht_opt$TITLE_PADDING)) {
        title_padding = ht_opt$TITLE_PADDING
    } else {
        title_padding = unit(c(0, 0), "points")
        title_padding[1] = title_padding[1] + unit(5.5, "points") + 
            convertHeight(grobDescent(textGrob(label = "jA", gp = gp)), "inches")
    }

    if(which == "row") {
        rot = switch(side,
            "left" = 90,
            "right" = 270)

        pushViewport(viewport(name = "global_row_title"))
        if("fill" %in% names(gp)) {
            grid.rect(gp = gpar(fill = gp$fill))
        }
        if(side == "left") {
            grid.text(title, x = unit(1, "npc") - title_padding[1], rot = rot, just = "bottom", gp = gp)
        } else {
            grid.text(title, x = title_padding[1], rot = rot, just = "bottom", gp = gp)
        }
        upViewport()
    } else {
        pushViewport(viewport(name = "global_column_title"))
        if("fill" %in% names(gp)) {
            grid.rect(gp = gpar(fill = gp$fill))
        }
        if(side == "top") {
            grid.text(title, y = title_padding[1], just = "bottom", gp = gp)
        } else {
            grid.text(title, y = unit(1, "npc") - title_padding[1], just = "top", gp = gp)
        }
        upViewport()
    }
})
