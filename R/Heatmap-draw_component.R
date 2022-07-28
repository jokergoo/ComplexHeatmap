
# == title
# Draw Heatmap Body
#
# == param
# -object A `Heatmap-class` object.
# -kr Row slice index.
# -kc Column slice index.
# -... Pass to `grid::viewport` which includes the slice of heatmap body.
#
# == details
# A viewport is created which contains subset rows and columns of the heatmap.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_heatmap_body",
    signature = "Heatmap",
    definition = function(object, kr = 1, kc = 1, ...) {

    if(ncol(object@matrix) == 0 || nrow(object@matrix) == 0) {
        return(invisible(NULL))
    }

    row_order = object@row_order_list[[kr]]
    column_order = object@column_order_list[[kc]]

    gp = object@matrix_param$gp
    border = object@matrix_param$border

    use_raster = object@heatmap_param$use_raster
    raster_device = object@heatmap_param$raster_device
    raster_quality = object@heatmap_param$raster_quality
    raster_device_param = object@heatmap_param$raster_device_param
    raster_by_magick = object@heatmap_param$raster_by_magick
    raster_magick_filter = object@heatmap_param$raster_magick_filter
    if(length(raster_device_param) == 0) raster_device_param = list()

    pushViewport(viewport(name = paste(object@name, "heatmap_body", kr, kc, sep = "_"), ...))

    mat = object@matrix[row_order, column_order, drop = FALSE]
    oe = try(col_matrix <- map_to_colors(object@matrix_color_mapping, mat), silent = TRUE)
    if(inherits(oe, "try-error")) {
        col_matrix = matrix(NA, nrow = nrow(mat), ncol = ncol(mat))
    }

    nc = ncol(mat)
    nr = nrow(mat)
    x = (seq_len(nc) - 0.5) / nc
    y = (rev(seq_len(nr)) - 0.5) / nr
    expand_index = expand.grid(seq_len(nr), seq_len(nc))
    
    cell_fun = object@matrix_param$cell_fun
    layer_fun = object@matrix_param$layer_fun
    if(!is.null(cell_fun)) {
        use_raster = FALSE
    }
    if(identical(object@matrix_param$gp$type, "none")) {
        use_raster = FALSE
    }
   
    if(use_raster) {

        # write the image into a temporary file and read it back
        device_info = switch(raster_device,
            png = c("grDevices", "png", "readPNG"),
            jpeg = c("grDevices", "jpeg", "readJPEG"),
            tiff = c("grDevices", "tiff", "readTIFF"),
            CairoPNG = c("Cairo", "png", "readPNG"),
            CairoJPEG = c("Cairo", "jpeg", "readJPEG"),
            CairoTIFF = c("Cairo", "tiff", "readTIFF"),
            agg_png = c("ragg", "png", "readPNG")
        )
        if(!requireNamespace(device_info[1])) {
            stop_wrap(paste0("Need ", device_info[1], " package to write image."))
        }
        if(!requireNamespace(device_info[2])) {
            stop_wrap(paste0("Need ", device_info[2], " package to read image."))
        }

        if(raster_device %in% c("png", "jpeg", "tiff")) {
            if(! "type" %in% names(raster_device_param)) {
                if(capabilities("cairo")) {
                    raster_device_param$type = "cairo"
                }
            }
        }

        # can we get the size of the heatmap body?
        heatmap_width_pt = max(1, ceiling(convertWidth(unit(1, "npc"), "bigpts", valueOnly = TRUE)))
        heatmap_height_pt = max(1, ceiling(convertHeight(unit(1, "npc"), "bigpts", valueOnly = TRUE)))

        if(raster_quality < 1) raster_quality = 1
        heatmap_width_pt = ceiling(heatmap_width_pt * raster_quality)
        heatmap_height_pt = ceiling(heatmap_height_pt * raster_quality)

        matrix_is_resized = FALSE
        # resize on the matrix
        raster_resize_mat = object@heatmap_param$raster_resize_mat
        if(!identical(raster_resize_mat, FALSE)) {
            if(is.logical(raster_resize_mat)) {
                raster_resize_mat_fun = function(x) mean(x, na.rm = TRUE)
            } else {
                if(!inherits(raster_resize_mat, "function")) {
                    stop_wrap("`raster_resize_mat` should be set as logical scalar or a function.")
                }
                raster_resize_mat_fun = raster_resize_mat
            }

            if(heatmap_width_pt < nc && heatmap_height_pt < nr) {
                mat2 = resize_matrix(mat, nr = heatmap_height_pt, nc = heatmap_width_pt, fun = raster_resize_mat_fun)
                matrix_is_resized = TRUE
            } else if(heatmap_width_pt < nc) {
                mat2 = resize_matrix(mat, nr = nr, nc = heatmap_width_pt, fun = raster_resize_mat_fun)
                matrix_is_resized = TRUE
            } else if(heatmap_height_pt < nr) {
                mat2 = resize_matrix(mat, nr = heatmap_height_pt, nc = nc, fun = raster_resize_mat_fun)
                matrix_is_resized = TRUE
            }
        }

        if(matrix_is_resized) {
            raster_by_magick = FALSE
        }

        temp_dir = tempdir()
        temp_image = tempfile(pattern = paste0(".heatmap_body_", digest::digest(object@name), "_", kr, "_", kc), tmpdir = temp_dir, fileext = paste0(".", device_info[2]))
        device_fun = getFromNamespace(raster_device, ns = device_info[1])

        if(raster_by_magick) {
            temp_image_width = ceiling(max(heatmap_width_pt, nc, 1))
            temp_image_height = ceiling(max(heatmap_height_pt, nr, 1))
        } else if(matrix_is_resized) {
            temp_image_width = ceiling(max(heatmap_width_pt, 1))
            temp_image_height = ceiling(max(heatmap_height_pt, 1))
        } else {
            temp_image_width = ceiling(max(heatmap_width_pt, 1))
            temp_image_height = ceiling(max(heatmap_height_pt, 1))
        }
        temp_image_width = as.integer(temp_image_width)
        temp_image_height = as.integer(temp_image_height)

        if(!is.na(ht_opt$raster_temp_image_max_width)) {
            temp_image_width = min(temp_image_width, ht_opt$raster_temp_image_max_width)
        }
        if(!is.na(ht_opt$raster_temp_image_max_height)) {
            temp_image_height = min(temp_image_height, ht_opt$raster_temp_image_max_height)
        }

        oe = try(do.call(device_fun, c(list(filename = temp_image, 
            width = temp_image_width, height = temp_image_height), raster_device_param)))
        if(inherits(oe, "try-error")) {
            stop_wrap(qq("The size of the temporary image for rasterization is too huge (@{temp_image_width} x @{temp_image_height} px) that it is cannot be handled by the device function `@{device_info[1]}:@{raster_device}()`. Please reduce the maximal size of temporary image by setting proper values for `ht_opt$raster_temp_image_max_width` and `ht_opt$raster_temp_image_max_height`."))
        }

        if(object@heatmap_param$verbose) {
            qqcat("saving into a temp image (.@{device_info[2]}) with size @{temp_image_width}x@{temp_image_height}px.\n")
        }
        if(matrix_is_resized) {
            if(object@heatmap_param$verbose) {
                qqcat("resize the matrix from (@{nrow(mat)}x@{ncol(mat)}) to (@{nrow(mat2)}x@{ncol(mat2)}).\n")
            }
            col_matrix2 = map_to_colors(object@matrix_color_mapping, mat2)
            nc2 = ncol(mat2)
            nr2 = nrow(mat2)
            x2 = (seq_len(nc2) - 0.5) / nc2
            y2 = (rev(seq_len(nr2)) - 0.5) / nr2
            expand_index2 = expand.grid(seq_len(nr2), seq_len(nc2))
            grid.rect(x2[expand_index2[[2]]], y2[expand_index2[[1]]], width = unit(1/nc2, 'npc'), height = unit(1/nr2, 'npc'), gp = do.call('gpar', c(list(fill = col_matrix2), gp)))
        } else {
            grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, 'npc'), height = unit(1/nr, 'npc'), gp = do.call('gpar', c(list(fill = col_matrix), gp)))
        }
        if(is.function(layer_fun)) {
            if(length(as.list(formals(layer_fun))) == 7) {
                layer_fun(column_order[ expand_index[[2]] ], row_order[ expand_index[[1]] ], 
                    unit(x[expand_index[[2]]], "npc"), unit(y[expand_index[[1]]], "npc"),
                    unit(rep(1/nc, nrow(expand_index)), "npc"), unit(rep(1/nr, nrow(expand_index)), "npc"),
                    as.vector(col_matrix))
            } else {
                layer_fun(column_order[ expand_index[[2]] ], row_order[ expand_index[[1]] ], 
                    unit(x[expand_index[[2]]], "npc"), unit(y[expand_index[[1]]], "npc"),
                    unit(rep(1/nc, nrow(expand_index)), "npc"), unit(rep(1/nr, nrow(expand_index)), "npc"),
                    as.vector(col_matrix), kr, kc)
            }
        }
        dev.off2()

        if(object@heatmap_param$verbose) {
            qqcat("resize the temp image to a size @{heatmap_width_pt}x@{heatmap_height_pt}px.\n")
        }
        if(raster_by_magick) {
            if(object@heatmap_param$verbose) {
                qqcat("image is read by magick.\n")
            }
            if(!requireNamespace("magick")) {
                stop_wrap("'magick' package should be installed.")
            }
            image = magick::image_read(temp_image)
            image = magick::image_resize(image, paste0(heatmap_width_pt, "x", heatmap_height_pt, "!"), filter = raster_magick_filter)
            image = as.raster(image)
        } else {
            if(object@heatmap_param$verbose) {
                qqcat("image is read by @{device_info[2]}::@{device_info[3]}\n")
            }
            image = getFromNamespace(device_info[3], ns = device_info[2])(temp_image)
        }
        # validate image, there might be white horizontal lines and vertical lines 
        # image = validate_raster_matrix(image, mat, object@matrix_color_mapping)

        grid.raster(image, width = unit(1, "npc"), height = unit(1, "npc"), interpolate = FALSE)

        ### only for testing the temp image size ###
        if(ht_opt("__export_image_size__")) {
            if(inherits(image, "magick-image")) {
                image = as.raster(image)
            } else {
                tf = tempfile()
                png(tf, width = heatmap_width_pt, height = heatmap_height_pt)
                grid.raster(image, width = unit(1, "npc"), height = unit(1, "npc"))
                dev.off()
                image = as.raster(png::readPNG(tf))
                file.remove(tf)   
            }
            attr(image, "width") = heatmap_width_pt
            attr(image, "height") = heatmap_height_pt
            # assign(".image", image, envir = .GlobalEnv)
        }
        ########################

        file.remove(temp_image)

    } else {

        if(any(names(gp) %in% c("type"))) {
            if(gp$type == "none") {
            } else {
                grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, "npc"), height = unit(1/nr, "npc"), gp = do.call("gpar", c(list(fill = col_matrix), gp)))
            }
        } else {
            grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, "npc"), height = unit(1/nr, "npc"), gp = do.call("gpar", c(list(fill = col_matrix), gp)))
        }

        if(is.function(cell_fun)) {
            for(i in seq_len(nr)) {
                for(j in seq_len(nc)) {
                    cell_fun(column_order[j], row_order[i], unit(x[j], "npc"), unit(y[i], "npc"), 
                        unit(1/nc, "npc"), unit(1/nr, "npc"), 
                        col_matrix[i, j])
                }
            }
        }
        if(is.function(layer_fun)) {
            if(length(as.list(formals(layer_fun))) == 7) {
                layer_fun(column_order[ expand_index[[2]] ], row_order[ expand_index[[1]] ], 
                    unit(x[expand_index[[2]]], "npc"), unit(y[expand_index[[1]]], "npc"),
                    unit(rep(1/nc, nrow(expand_index)), "npc"), unit(rep(1/nr, nrow(expand_index)), "npc"),
                    as.vector(col_matrix))
            } else {
                layer_fun(column_order[ expand_index[[2]] ], row_order[ expand_index[[1]] ], 
                    unit(x[expand_index[[2]]], "npc"), unit(y[expand_index[[1]]], "npc"),
                    unit(rep(1/nc, nrow(expand_index)), "npc"), unit(rep(1/nr, nrow(expand_index)), "npc"),
                    as.vector(col_matrix), kr, kc)
            }
        }
    }

    if(!(identical(border, FALSE) || identical(border, NA))) {
        border_gp = object@matrix_param$border_gp
        if(!identical(border, TRUE)) {
            border_gp$col = border
        }
        if("fill" %in% names(border_gp)) {
            message_wrap("`fill` is ignored in `border_gp`. The value for `fill` is always 'transparent'.")
        }
        border_gp$fill = "transparent"
        grid.rect(gp = border_gp)
    }

    upViewport()

})

# check white lines in the RGB matrix, and re-fill the color from mat with color_mapping
# careful: row orders of rgb and mat are reversed
# rgb: values change from 0 to 1
validate_raster_matrix = function(rgb, mat, col_mapping) {
    if(is.character(rgb)) {
        dim = dim(rgb)
        x = col2rgb(rgb)/255
        rgb = array(dim = c(dim, 3))
        rgb[, , 1] = matrix(x[1, ], nrow = dim[1], ncol = dim[2], byrow = TRUE)
        rgb[, , 2] = matrix(x[2, ], nrow = dim[1], ncol = dim[2], byrow = TRUE)
        rgb[, , 3] = matrix(x[3, ], nrow = dim[1], ncol = dim[2], byrow = TRUE)
    }
    # check rows
    white_row = NULL
    white_column = NULL
    if(any( abs(rgb[, 1, 1] - 1) < 1e-10 & abs(rgb[, 1, 2] - 1) < 1e-10 & abs(rgb[, 1, 3] - 1)< 1e-10 )) {
        white_row = which( abs(rowMeans(rgb[, , 1]) - 1) < 1e-10 & 
                           abs(rowMeans(rgb[, , 2]) - 1) < 1e-10 & 
                           abs(rowMeans(rgb[, , 3]) - 1) < 1e-10 )
    }
    if(any( abs(rgb[1, , 1] - 1) < 1e-10 & abs(rgb[1, , 2] - 1) < 1e-10 & abs(rgb[1, , 3] - 1)< 1e-10 )) {
        white_column = which( abs(colMeans(rgb[, , 1]) - 1) < 0.1 & 
                              abs(colMeans(rgb[, , 2]) - 1) < 0.1 & 
                              abs(colMeans(rgb[, , 3]) - 1) < 0.1 )
    }

    # estimate while rows and columns
    rgb
}

is_windows = function() {
    tolower(.Platform$OS.type) == "windows"
}

R_binary = function() {
    R_exe = ifelse(is_windows(), "R.exe", "R")
    return(file.path(R.home("bin"), R_exe))
}

# == title
# Draw Heatmap Dendrograms
#
# == param
# -object A `Heatmap-class` object.
# -which Are the dendrograms put on the row or on the column of the heatmap?
# -k Slice index.
# -max_height maximal height of dendrogram.
# -... Pass to `grid::viewport` which includes the complete heatmap dendrograms.
#
# == details
# A viewport is created which contains dendrograms.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == seealso
# `grid.dendrogram`
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_dend",
    signature = "Heatmap",
    definition = function(object,
    which = c("row", "column"), k = 1, max_height = NULL, ...) {

    which = match.arg(which)[1]
    
    side = switch(which,
        "row" = object@row_dend_param$side,
        "column" = object@column_dend_param$side)
    
    dend = switch(which,
        "row" = object@row_dend_list[[k]],
        "column" = object@column_dend_list[[k]])
    
    gp = switch(which,
        "row" = object@row_dend_param$gp,
        "column" = object@column_dend_param$gp)

    if(length(dend) == 0) {
        return(invisible(NULL))
    }

    if(is.null(dend)) return(invisible(NULL))

    if(nobs(dend) <= 1) {
        return(invisible(NULL))
    }

    if(is.null(max_height)) {
        max_height = dend_heights(dend)
    }
    if(max_height == 0) max_height = 1

    if(side %in% c("left", "right")) {
        xscale = c(0, max_height)
        yscale = c(0, nobs(dend))
        width = unit(1, "npc")
        height = unit(1, "npc")
        name = paste(object@name, "dend_row", k, sep = "_")
    } else {
        xscale = c(0, nobs(dend))
        yscale = c(0, max_height)
        height = unit(1, "npc")
        width = unit(1, "npc")
        name = paste(object@name, "dend_column", k, sep = "_")
    }

    pushViewport(viewport(...))
    pushViewport(viewport(name = name, xscale = xscale, yscale = yscale, width = width, height = height))

    if(side == "left") {
        grid.dendrogram(dend, gp = gp, facing = "right", order = "reverse")
    } else if(side == "right") {
        grid.dendrogram(dend, gp = gp, facing = "left", order = "reverse")
    } else if(side == "top") {
        grid.dendrogram(dend, gp = gp, facing = "bottom")
    } else if(side == "bottom") {
        grid.dendrogram(dend, gp = gp, facing = "top")
    } 

    upViewport()
    upViewport()

})

# == title
# Draw row names or column names
#
# == param
# -object A `Heatmap-class` object.
# -which Are the names put on the row or on the column of the heatmap?
# -k Slice index.
# -... Pass to `grid::viewport` which includes the complete heatmap row/column names.
#
# == details
# A viewport is created which contains row names or column names.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_dimnames",
    signature = "Heatmap",
    definition = function(object,
    which = c("row", "column"), k = 1, ...) {

    which = match.arg(which)[1]

    anno = switch(which,
        "row" = object@row_names_param$anno,
        "column" = object@column_names_param$anno)

    ind = switch(which,
        "row" = object@row_order_list[[k]],
        "column" = object@column_order_list[[k]])
    
    pushViewport(viewport(name = paste(object@name, which, "names", k, sep = "_"), ...))
    if(which == "row") {
        if(object@row_names_param$side == "right" ) {
            x = unit(0, "npc")
            y = unit(0.5, "npc")
            just = "left"
        } else {
            x = unit(1, "npc")
            y = unit(0.5, "npc")
            just = "right"
        }
    } else {
        if(object@column_names_param$side == "top") {
            x = unit(0.5, "npc")
            y = unit(0, "npc")
            just = "bottom"
        } else {
            x = unit(0.5, "npc")
            y = unit(1, "npc")
            just = "top"
        }
    }
    draw(anno, index = ind, x = x, y = y, just = just)
    upViewport()
})

# == title
# Draw Heatmap Title
#
# == param
# -object A `Heatmap-class` object.
# -which Is title put on the row or on the column of the heatmap?
# -k Slice index.
# -... Pass to `grid::viewport` which includes the complete heatmap title.
#
# == details
# A viewport is created which contains heatmap title.
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
    signature = "Heatmap",
    definition = function(object,
    which = c("row", "column"), k = 1, ...) {

    which = match.arg(which)[1]

    side = switch(which,
        "row" = object@row_title_param$side,
        "column" = object@column_title_param$side)

    gp = switch(which,
        "row" = object@row_title_param$gp,
        "column" = object@column_title_param$gp)
    
    gp = subset_gp(gp, k)
    
    title = switch(which,
        "row" = object@row_title[k],
        "column" = object@column_title[k])

    rot = switch(which,
        "row" = object@row_title_param$rot,
        "column" = object@column_title_param$rot)

    just = switch(which, 
        "row" = object@row_title_param$just,
        "column" = object@column_title_param$just)

    if(!is.null(ht_opt$TITLE_PADDING)) {
        title_padding = ht_opt$TITLE_PADDING
    } else {
        title_padding = unit(c(0, 0), "points")
        title_padding[1] = title_padding[1] + unit(5.5, "points") + 
            convertHeight(grobDescent(textGrob(label = "jA", gp = gp)), "inches")
    }

    if(which == "row") {
        
        pushViewport(viewport(name = paste(object@name, "row_title", k, sep = "_"), clip = FALSE, ...))
        gp2 = gp
        if("border" %in% names(gp2)) gp2$col = gp2$border
        if(any(c("border", "fill") %in% names(gp2))) grid.rect(gp = gp2)
        if(side == "left") {
            grid.text(title, x = unit(1, "npc") - title_padding[1], rot = rot, just = just, gp = gp)
        } else {
            grid.text(title, x = title_padding[1], rot = rot, just = just, gp = gp)
        }
        upViewport()
    } else {
        pushViewport(viewport(name = paste(object@name, "column_title", k, sep = "_"), clip = FALSE, ...))
        gp2 = gp
        if("border" %in% names(gp2)) gp2$col = gp2$border
        if(any(c("border", "fill") %in% names(gp2))) grid.rect(gp = gp2)
        if(side == "top") {
            grid.text(title, y = title_padding[1], rot = rot, just = just, gp = gp)
        } else {
            grid.text(title, y = unit(1, "npc") - title_padding[1], rot = rot, just = just, gp = gp)
        }
        upViewport()
    }
})

# == title
# Draw Heatmap Annotations on the Heatmap
#
# == param
# -object A `Heatmap-class` object.
# -which The position of the heamtap annotation.
# -k Slice index.
# -... Pass to `grid::viewport` which includes the complete heatmap annotation.
#
# == details
# A viewport is created which contains column/top annotations.
#
# The function calls `draw,HeatmapAnnotation-method` to draw the annotations.
#
# This function is only for internal use.
#
# == value
# This function returns no value.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
setMethod(f = "draw_annotation",
    signature = "Heatmap",
    definition = function(object, which = c("top", "bottom", "left", "right"), k = 1, ...) {
    
    which = match.arg(which)[1]

    annotation = switch(which,
        top = object@top_annotation,
        bottom = object@bottom_annotation,
        left = object@left_annotation,
        right = object@right_annotation)

    # if there is no annotation, draw nothing
    if(is.null(annotation)) {
        return(invisible(NULL))
    }

    if(which %in% c("top", "bottom")) {
        index = object@column_order_list[[k]]
        n = length(object@column_order_list)
    } else {
        index = object@row_order_list[[k]]
        n = length(object@row_order_list)
    }

    ## deal with the special anno_mark
    anno_mark_param = list()
    if(which %in% c("left", "right")) {
        slice_y = object@layout$slice$y
        n_slice = length(slice_y)
        slice_height = object@layout$slice$height

        if(n_slice > 1) {
            all_anno_type = anno_type(annotation)
            if(any(c("anno_zoom", "anno_mark") %in% all_anno_type)) {
                ## only make the anno_mark annotation
                ro_lt = object@row_order_list
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
                
                if(abs(par("din")[2] - grDevices::dev.size("in")[2]) < 1e-6) {
                    anno_mark_param$vp_height = convertHeight(unit(1, "npc"), "cm")  
                } else {
                    anno_mark_param$vp_height = convertHeight(unit(1, "npc"), "cm")*(par("din")[2]/grDevices::dev.size("in")[2])*0.9742
                }
                anno_mark_param$vp_width = unit(1, "npc")
                anno_mark_param$vp_just = "top"
                anno_mark_param$vp_x = unit(0.5, "npc")
                anno_mark_param$vp_y = unit(1, "npc")
            }
        }
    } else {
        slice_x = object@layout$slice$x
        n_slice = length(slice_x)
        slice_width = object@layout$slice$width

        if(n_slice > 1) {
            all_anno_type = anno_type(annotation)
            if(any(c("anno_zoom", "anno_mark") %in% all_anno_type)) {
                ## only make the anno_mark annotation
                co_lt = object@column_order_list
                .scale = c(0, 1)

                .pos = NULL
                for(i in seq_along(co_lt)) {
                    .pos1 = slice_x[i] + (seq_along(co_lt[[i]]) - 0.5)/length(co_lt[[i]]) * slice_width[i]
                    .pos1 = convertX(.pos1, "native", valueOnly = TRUE)
                    .pos = c(.pos, .pos1)
                }

                anno_mark_param$.scale = .scale
                anno_mark_param$.pos = .pos
                anno_mark_param$index = unlist(co_lt)
                
                anno_mark_param$vp_height = unit(1, "npc")
                if(abs(par("din")[1] - grDevices::dev.size("in")[1]) < 1e-6) {
                    anno_mark_param$vp_width = convertWidth(unit(1, "npc"), "cm")
                } else {
                    anno_mark_param$vp_width = convertWidth(unit(1, "npc"), "cm")*(par("din")[1]/grDevices::dev.size("in")[1])*0.945
                }
                anno_mark_param$vp_just = "left"
                anno_mark_param$vp_x = unit(0, "npc")
                anno_mark_param$vp_y = unit(0.5, "npc")
            }
        }
    }

    pushViewport(viewport(...))
    draw(annotation, index = index, k = k, n = n, anno_mark_param = anno_mark_param)
    upViewport()
})
