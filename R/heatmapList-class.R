
# == title
# class for a list of heatmaps
#
# == details
# The components for the heamtap list are placed into a 7 x 7 layout:
#
#          +------+
#          +------+
#          +------+
#    +-+-+-+------+-+-+-+
#    | | | |      | | | |
#    +-+-+-+------+-+-+-+
#          +------+
#          +------+
#          +------+
# 
# From top to bottom in column 4, the regions are:
#
# - annotation legend on the top, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
# - heatmap legend on the top, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - title for the heatmap list which are put on the top, graphics are drawn by `draw_title,HeatmapList-method`.
# - the heatmap list
# - title for the heatmap list which are put on the bottom, graphics are drawn by `draw_title,HeatmapList-method`.
# - heatmap legend on the bottom, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - annotation legend on the bottom, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
# 
# From left to right in row 4, the regions are:
#
# - annotation legend on the left, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
# - heatmap legend on the left, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - title for the heatmap list which are put on the left, graphics are drawn by `draw_title,HeatmapList-method`.
# - the heatmap list
# - title for the heatmap list which are put on the right, graphics are drawn by `draw_title,HeatmapList-method`.
# - heatmap legend on the right, graphics are drawn by `draw_heatmap_legend,HeatmapList-method`.
# - annotation legend on the right, graphics are drawn by `draw_annotation_legend,HeatmapList-method`.
#
# For the list of heatmaps which is placed at [5, 5] in the layout, the heatmaps are tiled one after the other.
#
# == methods
# The `HeatmapList` class provides following methods:
#
# - `draw,HeatmapList-method`: draw a single heatmap.
# - `add_heatmap,HeatmapList-method` add heatmaps to the list of heatmaps.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
HeatmapList = setClass("HeatmapList",
    slots = list(
        ht_list = "list",
        ht_list_param = "list",

        row_title = "character",
        row_title_param = "list",
        column_title = "character",
        column_title_param = "list",

        annotation_legend_param = "list",
        heatmap_legend_param = "list",

        layout = "environment"
    ),
    prototype = list(
        layout = as.environment(list(
            layout_annotation_legend_left_width = unit(0, "null"),
            layout_heatmap_legend_left_width = unit(0, "null"),
            layout_row_title_left_width = unit(0, "null"),
            layout_row_title_right_width = unit(0, "null"),
            layout_heatmap_legend_right_width = unit(0, "null"),
            layout_annotation_legend_right_width = unit(0, "null"),

            layout_annotation_legend_top_height = unit(0, "null"),
            layout_heatmap_legend_top_height = unit(0, "null"),
            layout_column_title_top_height = unit(0, "null"),
            layout_column_title_bottom_height = unit(0, "null"),
            layout_heatmap_legend_bottom_height = unit(0, "null"),
            layout_annotation_legend_bottom_height = unit(0, "null"),
            
            layout_index = matrix(nrow = 0, ncol = 2),
            graphic_fun_list = list()
        ))
    )
)

# == title
# Add heatmaps to the heatmap list
#
# == param
# -object a `HeatmapList` object.
# -ht a `Heatmap` object or a `HeatmapList` object.
#
# == details
# There is a shortcut function ``+.HeatmapList``.
#
# == value
# A `HeatmapList` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "add_heatmap",
    signature = "HeatmapList",
    definition = function(object, ht) {
    
    # check settings of this new heatmap
    if(inherits(ht, "Heatmap")) {
        ht_name = ht@name
        ht = list(ht)
        names(ht) = ht_name
    }

    # if ht is a HeatmapList, all settings are already checked
    object@ht_list = c(object@ht_list, ht)
    return(object)
})

# == title
# Make layout for the complete plot
#
# == param
# -object a `HeatmapList` object.
# -row_title title on the row.
# -row_title_side will the title be put on the left or right of the heatmap.
# -row_title_gp graphic parameters for drawing text.
# -column_title title on the column.
# -column_title_side will the title be put on the top or bottom of the heatmap.
# -column_title_gp graphic parameters for drawing text.
# -heatmap_legend_side side of the heatmap legend.
# -show_heatmap_legend whether show heatmap legend.
# -annotation_legend_side side of annotation legend.
# -show_annotation_legend whether show annotation legend.
# -hgap gap between heatmaps, should be a `grid::unit` object.
# -vgap gap between heatmaps, should be a `grid::unit` object.
# -auto_adjust auto adjust if the number of heatmap is larger than one.
#
# == detail
# It arranges components of the heatmap list and adjust graphic parameters if necessary.
#
# == value
# A `HeatmapList` object in which settings for each heatmap are adjusted.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "make_layout",
    signature = "HeatmapList",
    definition = function(object, row_title = character(0), 
    row_title_side = c("left", "right"), row_title_gp = gpar(fontsize = 14),
    column_title = character(0), column_title_side = c("top", "bottom"), 
    column_title_gp = gpar(fontsize = 14), 
    heatmap_legend_side = c("right", "left", "bottom", "top"), 
    show_heatmap_legend = TRUE,
    annotation_legend_side = c("right", "left", "bottom", "top"), 
    show_annotation_legend = TRUE,
    gap = unit(3, "mm"), auto_adjust = TRUE) {

    n = length(object@ht_list)
    object@ht_list[[1]] = prepare(object@ht_list[[1]])

    if(auto_adjust) {
        row_order = unlist(object@ht_list[[1]]@row_order_list)
        split = object@ht_list[[1]]@matrix_param$split
    	if(n > 1) {
    		for(i in seq_len(n-1)+1) {
                # some settings should be same as the first one
                object@ht_list[[i]]@matrix_param$km = 1
                object@ht_list[[i]]@row_hclust_param$show = FALSE
                object@ht_list[[i]]@row_hclust_param$cluster = TRUE
                object@ht_list[[i]] = prepare(object@ht_list[[i]], row_order = row_order, split = split)
                object@ht_list[[i]]@row_title = character(0)
    		}
    	}
    }

    if(n > 1) {
        if(length(gap) == 1) gap = rep(gap, n-1)
        gap = rep(gap, ceiling((n-1)/length(gap)))[seq_len(n-1)]
    } else {
        gap = unit(0, "mm")
    }
    object@ht_list_param$gap = gap

    object@layout$layout_index = rbind(c(4, 4))
    object@layout$graphic_fun_list = list(function(object) draw_heatmap_list(object))

    ############################################
    ## title on top or bottom
    column_title_side = match.arg(column_title_side)[1]
    if(length(column_title) == 0) {
        column_title = character(0)
    } else if(is.na(column_title)) {
        column_title = character(0)
    } else if(column_title == "") {
        column_title = character(0)
    }
    object@column_title = column_title
    object@column_title_param$gp = column_title_gp
    object@column_title_param$side = column_title_side
    if(length(column_title) > 0) {
        if(column_title_side == "top") {
            object@layout$layout_column_title_top_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(3, 4))
        } else {
            object@layout$layout_column_title_bottom_height = grobHeight(textGrob(column_title, gp = column_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(5, 4))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) object@draw_title(object, column_title, which = "column"))
    }

    ############################################
    ## title on left or right
    row_title_side = match.arg(row_title_side)[1]
    if(length(row_title) == 0) {
        row_title = character(0)
    } else if(is.na(row_title)) {
        row_title = character(0)
    } else if(row_title == "") {
        row_title = character(0)
    }
    object@row_title = row_title
    object@row_title_param$gp = row_title_gp
    object@row_title_param$side = row_title_side
    if(length(row_title) > 0) {
        if(row_title_side == "left") {
            object@layout$layout_row_title_left_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 3))
        } else {
            object@layout$layout_row_title_right_width = grobHeight(textGrob(row_title, gp = row_title_gp))*2
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 5))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_title(object, row_title, which = "row"))
    }

    #################################################
    ## heatmap legend to top, bottom, left and right
    # default values
    object@heatmap_legend_param$show = show_heatmap_legend
    if(show_heatmap_legend) {
        heatmap_legend_side = match.arg(heatmap_legend_side)[1]
        object@heatmap_legend_param$side = heatmap_legend_side
        if(heatmap_legend_side == "top") {
            object@layout$layout_heatmap_legend_top_height = heatmap_legend_size(object)[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(2, 4))
        } else if(heatmap_legend_side == "bottom") {
            object@layout$layout_heatmap_legend_bottom_height = heatmap_legend_size(object)[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(6, 4))
        } else if(heatmap_legend_side == "left") {
            object@layout$layout_heatmap_legend_left_width = heatmap_legend_size(object)[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 2))
        } else if(heatmap_legend_side == "right") {
            object@layout$layout_heatmap_legend_right_width = heatmap_legend_size(object)[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 6))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_heatmap_legend(object))
    }

    #################################################
    ## annotation legend to top, bottom, left and right
    # default values
    object@annotation_legend_param$show = show_annotation_legend
    if(show_annotation_legend) {
        annotation_legend_side = match.arg(annotation_legend_side)[1]
        object@annotation_legend_param$side = annotation_legend_side
        if(annotation_legend_side == "top") {
            object@layout$layout_annotation_legend_top_height = annotation_legend_size(object)[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(1, 4))
        } else if(annotation_legend_side == "bottom") {
            object@object@layout$layout_annotation_legend_bottom_height = annotation_legend_size(object)[2]
            object@layout$layout_index = rbind(object@layout$layout_index, c(7, 4))
        } else if(heatmap_legend_side == "left") {
            object@layout$layout_annotation_legend_left_width = annotation_legend_size(object)[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 1))
        } else if(annotation_legend_side == "right") {
            object@layout$layout_annotation_legend_right_width = annotation_legend_size(object)[1]
            object@layout$layout_index = rbind(object@layout$layout_index, c(4, 7))
        }
        object@layout$graphic_fun_list = c(object@layout$graphic_fun_list, function(object) draw_annotation_legend(object))
    }

    return(object)
})

# == title
# Draw a list of heatmaps
#
# == param
# -object a `HeatmapList` object
# -... pass to `make_layout,HeatmapList-method`
# -newpage whether to create a new page
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw",
    signature = "HeatmapList",
    definition = function(object, ..., newpage= TRUE) {
    
    if(newpage) {
        grid.newpage()
    }
    
    object = make_layout(object, ...)

    layout = grid.layout(nrow = 7, ncol = 7, widths = component_width(object, 1:7), heights = component_height(object, 1:7))
    pushViewport(viewport(layout = layout, name = "global"))
    ht_layout_index = object@layout$layout_index
    ht_graphic_fun_list = object@layout$graphic_fun_list
    
    for(j in seq_len(nrow(ht_layout_index))) {
        pushViewport(viewport(layout.pos.row = ht_layout_index[j, 1], layout.pos.col = ht_layout_index[j, 2]))
        ht_graphic_fun_list[[j]](object)
        upViewport()
    }

    upViewport()
})

# == title
# Width of each heatmap list component
#
# == param
# -object a `HeatmapList` object.
# -k which components, see `HeatmapList-class`.
#
# == value
# A `grid::unit` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_width",
    signature = "HeatmapList",
    definition = function(object, k = 1:7) {

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_annotation_legend_left_width
        } else if(k == 2) {
            object@layout$layout_heatmap_legend_left_width
        } else if(k == 3) {
            object@layout$layout_row_title_left_width
        } else if(k == 4) {
            unit(1, "null")
        } else if(k == 5) {
            object@layout$layout_row_title_right_width
        } else if(k == 6) {
            object@layout$layout_heatmap_legend_right_width
        } else if(k == 7) {
            object@layout$layout_annotation_legend_right_width
        } else {
            stop("wrong 'k'")
        }
    }

    do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})

# == title
# Height of each heatmap list component
#
# == param
# -object a `HeatmapList` object.
# -k which components, see `HeatmapList-class`.
#
# == value
# A `grid::unit` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "component_height",
    signature = "HeatmapList",
    definition = function(object, k = 1:7) {

    .single_unit = function(k) {
        if(k == 1) {
            object@layout$layout_annotation_legend_top_height
        } else if(k == 2) {
            object@layout$layout_heatmap_legend_top_height
        } else if(k == 3) {
            object@layout$layout_column_title_top_height
        } else if(k == 4) {
            unit(1, "null")
        } else if(k == 5) {
            object@layout$layout_column_title_bottom_height
        } else if(k == 6) {
            object@layout$layout_heatmap_legend_bottom_height
        } else if(k == 7) {
            object@layout$layout_annotation_legend_bottom_height
        } else {
            stop("wrong 'k'")
        }
    }

    do.call("unit.c", lapply(k, function(i) .single_unit(i)))
})


# == title
# Draw the list of heatmaps
#
# == param
# -object a `HeatmapList` object
# -hgap gap
#
# == details
# A viewport is created which contains heatmaps.
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

    gap = object@ht_list_param$gap

    # since each heatmap actually has nine rows, calculate the maximum height of corresponding rows in all heatmap 
    max_component_height = unit.c(
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 1)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 2)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 3)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 4)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 5)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 6)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 7)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 8)))),
        max(do.call("unit.c", lapply(object@ht_list, function(ht) component_height(ht, k = 9))))
    )

    # set back to each heatmap
    n = length(object@ht_list)
    for(i in seq_len(n)) {
        set_component_height(object@ht_list[[i]], k = 1, max_component_height[1])
        set_component_height(object@ht_list[[i]], k = 2, max_component_height[2])
        set_component_height(object@ht_list[[i]], k = 3, max_component_height[3])
        set_component_height(object@ht_list[[i]], k = 4, max_component_height[4])
        set_component_height(object@ht_list[[i]], k = 6, max_component_height[6])
        set_component_height(object@ht_list[[i]], k = 7, max_component_height[7])
        set_component_height(object@ht_list[[i]], k = 8, max_component_height[8])
        set_component_height(object@ht_list[[i]], k = 9, max_component_height[9])
    }

    width_without_heatmap_body = do.call("unit.c", lapply(object@ht_list, function(ht) component_width(ht, c(1:3, 5:7))))
    heatmap_ncol = sapply(object@ht_list, function(ht) ncol(ht@matrix))

    # width for body for each heatmap
    heatmap_body_width = (unit(1, "npc") - sum(width_without_heatmap_body) - sum(gap)) * (1/sum(heatmap_ncol)) * heatmap_ncol

    # width of heatmap including body, and other components
    heatmap_width = sum(width_without_heatmap_body[1:3]) + heatmap_body_width[1] + sum(width_without_heatmap_body[5:7-1])

    for(i in seq_len(n - 1) + 1) {
        heatmap_width = unit.c(heatmap_width, sum(width_without_heatmap_body[6*(i-1) + 1:3]) + heatmap_body_width[i] + sum(width_without_heatmap_body[6*(i-1) + 5:7-1]))
    }

    pushViewport(viewport(name = "main_heatmap_list"))
    
    x = unit(0, "npc")
    for(i in seq_len(n)) {
        pushViewport(viewport(x = x, y = unit(0, "npc"), width = heatmap_width[i], just = c("left", "bottom"), name = paste0("heatmap_", object@ht_list[[i]]@name)))
        ht = object@ht_list[[i]]
        draw(ht, internal = TRUE)
        upViewport()

        if(i < n) {
        	x = x + sum(heatmap_width[seq_len(i)]) + sum(gap[seq_len(i)])
        }
    }

    upViewport()

})

# == title
# Draw heatmap list title
#
# == param
# -object a `HeatmapList` object
# -title title
# -side side of heatmap title.
# -gp graphic paramter for drawing text.
#
# == details
# A viewport is created which contains heatmap list title.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_title",
    signature = "HeatmapList",
    definition = function(object, title,
    which = c("column", "row"), ...) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_title_param$side,
        "column" = object@column_title_param$side)

    gp = switch(which,
        "row" = object@row_title_param$gp,
        "column" = object@column_title_param$gp)

    if(which == "row") {
        rot = switch(side,
            "left" = 90,
            "right" = 270)

        pushViewport(viewport(name = "global_row_title", clip = FALSE, ...))
        grid.text(title, rot = rot, gp = gp)
        upViewport()
    } else {
        pushViewport(viewport(name = "global_column_title", clip = FALSE, ...))
        grid.text(title, gp = gp)
        upViewport()
    }
})

# == title
# Draw legends for all heatmaps
#
# == param
# -object a `HeatmapList` object
# -side side
#
# == details
# A viewport is created which contains heatmap legends.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_legend",
    signature = "HeatmapList",
    definition = function(object, ...) {

    side = object@heatmap_legend_param$side

    ColorMappingList = lapply(object@ht_list, function(ht) ht@matrix_color_mapping)
    draw_legend(ColorMappingList, side = side, ...)
})

# == title
# Draw legends for all column annotations
#
# == param
# -object a `HeatmapList` object
# -side side
#
# == details
# A viewport is created which contains annotation legends.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_annotation_legend",
    signature = "HeatmapList",
    definition = function(object, ...) {

    side = object@heatmap_legend_param$side

    ColorMappingList = do.call("c", lapply(object@ht_list, function(ht) ht@column_anno_color_mapping))
    nm = names(ColorMappingList)
    ColorMappingList = ColorMappingList[nm]
    draw_legend(ColorMappingList, side = side, ...)
})

# == title
# Size of the heatmap legend viewprot
#
# == param
# -object a `HeatmapList` object
# -side side
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "heatmap_legend_size",
    signature = "HeatmapList",
    definition = function(object) {

    side = object@heatmap_legend_param$side

    ColorMappingList = lapply(object@ht_list, function(ht) ht@matrix_color_mapping)
    draw_legend(ColorMappingList, side = side, plot = FALSE)
})

# == title
# Size of the annotation legend viewport
#
# == param
# -object a `HeatmapList` object
# -side side
# -vp_width vp_width
# -vp_height vp_height
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "annotation_legend_size",
    signature = "HeatmapList",
    definition = function(object, 
    vp_width = unit(1, "npc"), vp_height = unit(1, "npc")) {

    side = object@annotation_legend_param$side

    ColorMappingList = do.call("c", lapply(object@ht_list, function(ht) ht@column_anno_color_mapping))
    nm = names(ColorMappingList)
    ColorMappingList = ColorMappingList[nm]
    draw_legend(ColorMappingList, side = side, plot = FALSE, vp_width = vp_width, vp_height = vp_height)
})

draw_legend = function(ColorMappingList, side = c("right", "left", "top", "bottom"), plot = TRUE,
    vp_width = unit(1, "npc"), vp_height = unit(1, "npc"), gap = unit(2, "mm"), 
    padding = unit(4, "mm")) {

    side = match.arg(side)[1]

    n = length(ColorMappingList)

    if(side %in% c("left", "right")) {
    	if(side == "left") {
        	current_x = unit(0, "npc")
        } else {
        	current_x = unit(0, "npc") + padding
        }
        current_width = unit(0, "null")
        current_y = vp_height
        for(i in seq_len(n)) {
            cm = ColorMappingList[[i]]
            size = color_mapping_legend(cm, plot = FALSE)
            # if this legend is too long that it exceed the bottom of the plotting region
            # it also works for the first legend if it is too long
            #if(compare_unit(current_y - size[2], unit(0, "npc")) < 0) {
            if(0){
                # go to next column
                current_y = unit(1, "npc")
                current_x = current_width
                current_width = current_x + size[1]

                if(plot) color_mapping_legend(cm, x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
                current_y = current_y - size[2] # move to the bottom
            } else {
                # if this legend is wider
                if(compare_unit(current_width, current_x + size[1]) < 0) {
                    current_width = current_x + size[1]
                }

                if(plot) color_mapping_legend(cm, x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
                current_y = current_y - size[2] - gap # move to the bottom
            }
        }

        if(side == "left") {
        	current_width = current_width + padding
        }

        return(unit.c(current_width, vp_height))

    } else if(side %in% c("top", "bottom")) {
        current_x = unit(0, "npc")
        if(side == "top") {
        	current_height = vp_height
        } else {
        	current_height = vp_height - padding
        }
        current_y = vp_height
        for(i in seq_len(n)) {
            cm = ColorMappingList[[i]]
            size = color_mapping_legend(cm, plot = FALSE)
            
            # if adding the legend exceeding ...
            #if(compare_unit(current_x + size[1], vp_width) > 0) {
            if(0) {
                # go to next column
                current_y = unit(1, "npc") - current_height
                current_x = unit(0, "npc")
                current_height = current_y - size[2]

                if(plot) color_mapping_legend(cm, x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
                current_x = current_x + size[1]
            } else {
                # if height of this legend is larger
                if(compare_unit(current_height, current_y - size[2]) > 0) {
                    current_height = current_y - size[2]
                }

                if(plot) color_mapping_legend(cm, x = current_x, y = current_y, just = c("left", "top"), plot = TRUE)
                current_x = current_x + size[1] + gap
            }
        }
        if(side == "top") {
        	current_height = current_height - padding
        }

        return(unit.c(vp_width, unit(1, "npc") - current_height))
        
    }
}

setMethod(f = "show",
    signature = "HeatmapList",
    definition = function(object) {

    cat("A HeatmapList object containing", length(object@ht_list), "heatmaps:\n\n")
    for(i in seq_along(object@ht_list)) {
        cat("[", i, "] ", sep = "")
        show(object@ht_list[[i]])
        cat("\n")
    }
})

# == title
# Add heatmaps to the list
#
# == param
# -ht1 a `HeatmapList` object.
# -ht2 a `Heatmap` object or a `HeatmapList` object.
#
# == value
# A `HeatmapList` object
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
"+.HeatmapList" = function(ht1, ht2) {
    if(inherits(ht2, "Heatmap") || inherits(ht2, "HeatmapList")) {
        add_heatmap(ht1, ht2)
    } else {
        stop("`ht2` should be a `Heatmap` or `HeatmapList` object.")

    }
}


compare_unit = function(u1, u2) {
    u1 = convertUnit(u1, "cm", valueOnly = TRUE)
    u2 = convertUnit(u2, "cm", valueOnly = TRUE)
    ifelse(u1 > u2, 1, ifelse(u1 < u2, -1, 0))
}
