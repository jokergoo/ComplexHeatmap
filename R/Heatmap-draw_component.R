
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
    if(length(raster_device_param) == 0) raster_device_param = list()

    pushViewport(viewport(name = paste(object@name, "heatmap_body", kr, kc, sep = "_"), ...))

    mat = object@matrix[row_order, column_order, drop = FALSE]
    col_matrix = map_to_colors(object@matrix_color_mapping, mat)

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
        
    if(use_raster) {
        # write the image into a temporary file and read it back
        device_info = switch(raster_device,
            png = c("grDevices", "png", "readPNG"),
            jpeg = c("grDevices", "jpeg", "readJPEG"),
            tiff = c("grDevices", "tiff", "readTIFF"),
            CairoPNG = c("Cairo", "png", "readPNG"),
            CairoJPEG = c("Cairo", "jpeg", "readJPEG"),
            CairoTIFF = c("Cairo", "tiff", "readTIFF")
        )
        if(!requireNamespace(device_info[1])) {
            stop_wrap(paste0("Need ", device_info[1], " package to write image."))
        }
        if(!requireNamespace(device_info[2])) {
            stop_wrap(paste0("Need ", device_info[2], " package to read image."))
        }
        # can we get the size of the heatmap body?
        heatmap_width = convertWidth(unit(1, "npc"), "bigpts", valueOnly = TRUE)
        heatmap_height = convertHeight(unit(1, "npc"), "bigpts", valueOnly = TRUE)
        if(heatmap_width <= 0 || heatmap_height <= 0) {
            stop_wrap("The width or height of the raster image is zero, maybe you forget to turn off the previous graphic device or it was corrupted. Run `dev.off()` to close it.")
        }
        
        temp_dir = tempdir()
                # dir.create(tmp_dir, showWarnings = FALSE)
        temp_image = tempfile(pattern = paste0(".heatmap_body_", object@name, "_", kr, "_", kc), tmpdir = temp_dir, fileext = paste0(".", device_info[2]))
        #getFromNamespace(raster_device, ns = device_info[1])(temp_image, width = heatmap_width*raster_quality, height = heatmap_height*raster_quality)
        device_fun = getFromNamespace(raster_device, ns = device_info[1])

        do.call(device_fun, c(list(filename = temp_image, width = max(c(heatmap_width*raster_quality, 1)), height = max(c(heatmap_height*raster_quality, 1))), raster_device_param))
        grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, 'npc'), height = unit(1/nr, 'npc'), gp = do.call('gpar', c(list(fill = col_matrix), gp)))
        if(is.function(layer_fun)) {
            layer_fun(column_order[ expand_index[[2]] ], row_order[ expand_index[[1]] ], 
                x[expand_index[[2]]], y[expand_index[[1]]],
                unit(rep(1/nc, nrow(expand_index)), "npc"), unit(rep(1/nr, nrow(expand_index)), "npc"),
                as.vector(col_matrix))
        }
        dev.off2()
        
        # ############################################
        # ## make the heatmap body in a another process
        # temp_R_data = tempfile(pattern = paste0(".heatmap_body_", object@name, "_", kr, "_", kc), tmpdir = temp_dir, fileext = paste0(".RData"))
        # temp_R_file = tempfile(pattern = paste0(".heatmap_body_", object@name, "_", kr, "_", kc), tmpdir = temp_dir, fileext = paste0(".R"))
        # if(Sys.info()["sysname"] == "Windows") {
        #     temp_image = gsub("\\\\", "/", temp_image)
        #     temp_R_data = gsub("\\\\", "/", temp_R_data)
        #     temp_R_file = gsub("\\\\", "/", temp_R_file)
        # }
        # save(device_fun, device_info, temp_image, heatmap_width, raster_quality, heatmap_height, raster_device_param,
        #     gp, x, expand_index, nc, nr, col_matrix, row_order, column_order, y,
        #     file = temp_R_data)
        # R_cmd = qq("
        # library(@{device_info[1]})
        # library(grid)
        # load('@{temp_R_data}')
        # do.call('device_fun', c(list(filename = temp_image, width = max(c(heatmap_width*raster_quality, 1)), height = max(c(heatmap_height*raster_quality, 1))), raster_device_param))
        # grid.rect(x[expand_index[[2]]], y[expand_index[[1]]], width = unit(1/nc, 'npc'), height = unit(1/nr, 'npc'), gp = do.call('gpar', c(list(fill = col_matrix), gp)))
        # dev.off()
        # q(save = 'no')
        # ", code.pattern = "@\\{CODE\\}")
        # writeLines(R_cmd, con = temp_R_file)
        # if(grepl(" ", temp_R_file)) {
        #     if(is_windows()) {
        #         oe = try(system(qq("\"@{normalizePath(R_binary(), winslash='/')}\" --vanilla < \'@{temp_R_file}\'", code.pattern = "@\\{CODE\\}"), ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE), silent = TRUE)
        #     } else {
        #         oe = try(system(qq("\"@{normalizePath(R_binary(), winslash='/')}\" --vanilla < \'@{temp_R_file}\'", code.pattern = "@\\{CODE\\}"), ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
        #     }
        # } else {
        #     if(is_windows()) {
        #         oe = try(system(qq("\"@{normalizePath(R_binary(), winslash='/')}\" --vanilla < @{temp_R_file}", code.pattern = "@\\{CODE\\}"), ignore.stdout = TRUE, ignore.stderr = TRUE, show.output.on.console = FALSE), silent = TRUE)
        #     } else {
        #         oe = try(system(qq("\"@{normalizePath(R_binary(), winslash='/')}\" --vanilla < @{temp_R_file}", code.pattern = "@\\{CODE\\}"), ignore.stdout = TRUE, ignore.stderr = TRUE), silent = TRUE)
        #     }
        # }
        # ############################################
        # file.remove(temp_R_data)
        # file.remove(temp_R_file)
        # if(inherits(oe, "try-error")) {
        #     stop(oe)
        # }
        image = getFromNamespace(device_info[3], ns = device_info[2])(temp_image)
        image = as.raster(image)
        grid.raster(image, width = unit(1, "npc"), height = unit(1, "npc"))
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
            layer_fun(column_order[ expand_index[[2]] ], row_order[ expand_index[[1]] ], 
                x[expand_index[[2]]], y[expand_index[[1]]],
                unit(rep(1/nc, nrow(expand_index)), "npc"), unit(rep(1/nr, nrow(expand_index)), "npc"),
                as.vector(col_matrix))
        }
    }

    if(!identical(border, FALSE)) {
        grid.rect(gp = gpar(fill = "transparent", col = border))
    }

    upViewport()

})

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
    draw(anno, index = ind)
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

    if(which == "row") {
        
        pushViewport(viewport(name = paste(object@name, "row_title", k, sep = "_"), clip = FALSE, ...))
        if("fill" %in% names(gp)) {
            grid.rect(gp = gpar(fill = gp$fill))
        }
        if(side == "left") {
            grid.text(title, x = unit(1, "npc") - ht_opt$TITLE_PADDING, rot = rot, just = just, gp = gp)
        } else {
            grid.text(title, x = ht_opt$TITLE_PADDING, rot = rot, just = just, gp = gp)
        }
        upViewport()
    } else {
        pushViewport(viewport(name = paste(object@name, "column_title", k, sep = "_"), clip = FALSE, ...))
        if("fill" %in% names(gp)) {
            grid.rect(gp = gpar(fill = gp$fill))
        }
        if(side == "top") {
            grid.text(title, y = ht_opt$TITLE_PADDING, rot = rot, just = just, gp = gp)
        } else {
            grid.text(title, y = unit(1, "npc") - ht_opt$TITLE_PADDING, rot = rot, just = just, gp = gp)
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

    pushViewport(viewport(...))
    draw(annotation, index = index, k = k, n = n)
    upViewport()
})
