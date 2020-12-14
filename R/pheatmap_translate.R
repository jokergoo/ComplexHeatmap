# == title
# Translate pheatmap::pheatmap to ComplexHeatmap::Heatmap
#
# == param
# -mat The input matrix.
# -color The same as in `pheatmap::pheatmap`. Here you don't necessarily need to generate a long color vector.
#        The discrete colors sent to `grDevices::colorRampPalette` are also OK here. E.g.
#        ``colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100)`` can be simply 
#        replaced as ``rev(brewer.pal(n = 7, name = "RdYlBu"))``.
# -kmeans_k Not supported.
# -breaks The same as in `pheatmap::pheatmap`.
# -border_color The same as in `pheatmap::pheatmap`.
# -cellwidth The same as in `pheatmap::pheatmap`.
# -cellheight The same as in `pheatmap::pheatmap`.
# -scale The same as in `pheatmap::pheatmap`.
# -cluster_rows The same as in `pheatmap::pheatmap`.
# -cluster_cols The same as in `pheatmap::pheatmap`.
# -clustering_distance_rows The same as in `pheatmap::pheatmap`.
# -clustering_distance_cols The same as in `pheatmap::pheatmap`.
# -clustering_method The same as in `pheatmap::pheatmap`.
# -clustering_callback The same as in `pheatmap::pheatmap`.
# -cutree_rows The same as in `pheatmap::pheatmap`.
# -cutree_cols The same as in `pheatmap::pheatmap`.
# -treeheight_row The same as in `pheatmap::pheatmap`.
# -treeheight_col The same as in `pheatmap::pheatmap`.
# -legend The same as in `pheatmap::pheatmap`.
# -legend_breaks The same as in `pheatmap::pheatmap`.
# -legend_labels The same as in `pheatmap::pheatmap`.
# -annotation_row The same as in `pheatmap::pheatmap`.
# -annotation_col The same as in `pheatmap::pheatmap`.
# -annotation The same as in `pheatmap::pheatmap`.
# -annotation_colors The same as in `pheatmap::pheatmap`.
# -annotation_legend The same as in `pheatmap::pheatmap`.
# -annotation_names_row The same as in `pheatmap::pheatmap`.
# -annotation_names_col The same as in `pheatmap::pheatmap`.
# -drop_levels Enforced to be ``TRUE``.
# -show_rownames The same as in `pheatmap::pheatmap`.
# -show_colnames The same as in `pheatmap::pheatmap`.
# -main The same as in `pheatmap::pheatmap`.
# -fontsize The same as in `pheatmap::pheatmap`.
# -fontsize_row The same as in `pheatmap::pheatmap`.
# -fontsize_col The same as in `pheatmap::pheatmap`.
# -angle_col The same as in `pheatmap::pheatmap`.
# -display_numbers The same as in `pheatmap::pheatmap`.
# -number_format The same as in `pheatmap::pheatmap`.
# -number_color The same as in `pheatmap::pheatmap`.
# -fontsize_number The same as in `pheatmap::pheatmap`.
# -gaps_row The same as in `pheatmap::pheatmap`.
# -gaps_col The same as in `pheatmap::pheatmap`.
# -labels_row The same as in `pheatmap::pheatmap`.
# -labels_col The same as in `pheatmap::pheatmap`.
# -filename Not supported.
# -width Not supported.
# -height Not supported.
# -silent Not supported.
# -na_col The same as in `pheatmap::pheatmap`.
# -name Name of the heatmap. This argument is passed to `Heatmap`.
# -heatmap_legend_param  Pass to `Heatmap`.
# -... Other arguments passed to `Heatmap`.
#
# == details
# This function aims to execute ``pheatmap::pheatmap`` code purely with ComplexHeatmap.
#
# == seealso
# See https://jokergoo.github.io/2020/05/06/translate-from-pheatmap-to-complexheatmap/
#
# `compare_pheatmap` that compares heatmaps between ``pheatmap::pheatmap()`` and ``ComplexHeatmap::pheatmap()``.
#
# == value
# A `Heatmap-class` object.
#
pheatmap = function(mat, 
    color = colorRampPalette(rev(brewer.pal(n = 7, name = "RdYlBu")))(100), 
    kmeans_k = NA, 
    breaks = NA, 
    border_color = "grey60",
    cellwidth = NA, 
    cellheight = NA, 
    scale = "none", 
    cluster_rows = TRUE,
    cluster_cols = TRUE, 
    clustering_distance_rows = "euclidean",
    clustering_distance_cols = "euclidean", 
    clustering_method = "complete",
    clustering_callback = NA, 
    cutree_rows = NA, 
    cutree_cols = NA,
    treeheight_row = ifelse(class(cluster_rows) == "hclust" || cluster_rows, 50, 0), 
    treeheight_col = ifelse(class(cluster_cols) == "hclust" || cluster_cols, 50, 0), 
    legend = TRUE, 
    legend_breaks = NA,
    legend_labels = NA, 
    annotation_row = NA, 
    annotation_col = NA,
    annotation = NA, 
    annotation_colors = NA, 
    annotation_legend = TRUE,
    annotation_names_row = TRUE, 
    annotation_names_col = TRUE,
    drop_levels = TRUE, 
    show_rownames = TRUE, 
    show_colnames = TRUE, 
    main = NA,
    fontsize = 10, 
    fontsize_row = fontsize, 
    fontsize_col = fontsize,
    angle_col = c("270", "0", "45", "90", "315"), 
    display_numbers = FALSE,
    number_format = "%.2f", 
    number_color = "grey30", 
    fontsize_number = 0.8 * fontsize, 
    gaps_row = NULL, 
    gaps_col = NULL, 
    labels_row = NULL,
    labels_col = NULL, 
    filename = NA, 
    width = NA, 
    height = NA,
    silent = FALSE, 
    na_col = "#DDDDDD", 
    name = " ",

    # argument specific for Heatmap()
    heatmap_legend_param = list(),
    ...
) {

  
    if(is.data.frame(mat)) {
        warning_wrap("The input is a data frame, convert it to the matrix.")
        mat = as.matrix(mat)
    }

    if("row" %in% scale) {

        if(any(is.na(mat))) {
            mat = (mat - rowMeans(mat, na.rm = TRUE))/rowSds(mat, na.rm = TRUE)
        } else {
            mat = t(scale(t(mat)))
        }
    } else if("column" %in% scale) {
        if(any(is.na(mat))) {
            mat = t((t(mat) - colMeans(mat, na.rm = TRUE))/colSds(mat, na.rm = TRUE))
        } else {
            mat = scale(mat)
        }
    }

    ht_param = list(matrix = mat)

    if(!identical(scale, "none") && !identical(breaks, NA)) {
        warning_wrap("It not suggested to both set `scale` and `breaks`. It makes the function confused.")
    }

    # if color is a color mapping function
    if(is.function(color)) {
        ht_param$col = color
        if(!identical(breaks, NA)) {
            warning_wrap("`breaks` is ignored when `color` is set as a color mapping function.")
        }
    } else {
        if(identical(breaks, NA)) {
            n_col = length(color)
            if(identical(scale, "row") || identical(scale, "column")) {
                lim = max(abs(mat), na.rm = TRUE)
                ht_param$col = colorRamp2(seq(-lim, lim, length = n_col), color)
            } else {
                ht_param$col = colorRamp2(seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE), length = n_col), color)
            }
        } else  {
            if(length(breaks) == length(color) + 1) {
                ht_param$col = local({
                    breaks = breaks
                    color = color
                    fun = function(x) {
                        n = length(color)
                        df = data.frame(start = c(-Inf, breaks[seq_len(n)], breaks[n+1]), 
                                        end = c(breaks[1], breaks[1+seq_len(n)], Inf))
                        # tell which interval x is in
                        ind = numeric(length(x))
                        for(i in seq_along(x)) {
                            ind[i] = which(df$start <= x[i] & df$end > x[i])
                        }
                        ind = ind - 1
                        ind[ind < 1] = 1
                        ind[ind > n] = n
                        color[ind]
                    }
                    attr(fun, "breaks") = breaks
                    fun
                })
            } else if(length(breaks) == length(color)) {
                ht_param$col = colorRamp2(breaks, color)
            } else {
                n_col = length(color)
                ht_param$col  = colorRamp2(seq(min(breaks), max(breaks), length = n_col), color)
                warning_wrap("`breaks` does not have the same length as `color`. The colors are interpolated from the minimal to the maximal of `breaks`.")
            }
        }
    }

    if(!identical(kmeans_k, NA)) {
        warning_wrap("argument `kmeans_k` is not supported in pheatmap -> Heatmap translation, skip it. You might check `row_km` and `column_km` arguments in Heatmap().")
    }
    
    if(!identical(filename, NA)) {
        warning_wrap("argument `filename` is not supported in pheatmap -> Heatmap translation, skip it.")
    }

    if(!identical(width, NA)) {
        warning_wrap("argument `width` is not supported in pheatmap -> Heatmap translation, skip it.")
    }
    
    if(!identical(height, NA)) {
        warning_wrap("argument `height` is not supported in pheatmap -> Heatmap translation, skip it.")
    }

    if(!identical(silent, FALSE)) {
        warning_wrap("argument `silent` is not supported in pheatmap -> Heatmap translation, skip it.")
    }

    ht_param$rect_gp = gpar(col = border_color)

    if(nrow(mat) > 1000 || ncol(mat) > 1000) {
        if(!is.na(border_color)) {
            warning_wrap("border color is set for the matrix with large numbers of rows or columns. You might only be able to see the border colors in the plot. Set `border_color = NA` to get rid of it.")
        }
    }

    if(!identical(cellwidth, NA)) {
        ht_param$width = ncol(mat)*unit(cellwidth, "pt")
    }

    if(!identical(cellheight, NA)) {
        ht_param$height = nrow(mat)*unit(cellheight, "pt")
    }

    if(identical(clustering_distance_rows, "correlation")) clustering_distance_rows = "pearson"
    if(identical(clustering_distance_cols, "correlation")) clustering_distance_cols = "pearson"

    ht_param$cluster_rows = cluster_rows
    ht_param$cluster_columns = cluster_cols 
    ht_param$clustering_distance_rows = clustering_distance_rows
    ht_param$clustering_distance_columns = clustering_distance_cols 
    ht_param$clustering_method_rows = clustering_method
    ht_param$clustering_method_columns = clustering_method

    if(!is.na(cutree_rows)) {
        if(inherits(cluster_rows, c("logical", "hclust", "dendrogram"))) {
            ht_param$row_split = cutree_rows
            ht_param$row_gap = unit(4, "bigpts")
            ht_param["row_title"] = list(NULL)
        }
    }
    if(!is.na(cutree_cols)) {
        if(inherits(cluster_cols, c("logical", "hclust", "dendrogram"))) {
            ht_param$column_split = cutree_cols
            ht_param$column_gap = unit(4, "bigpts")
            ht_param["column_title"] = list(NULL)
        }
    }
    
    ht_param$row_dend_width = unit(treeheight_row, "pt")
    ht_param$column_dend_height = unit(treeheight_col, "pt")

    ht_param$show_heatmap_legend = legend

    if(identical(scale, "row") || identical(scale, "column")) {
        if(identical(legend_breaks, NA)) {
            lim = quantile(abs(mat), 0.975)

            le = pretty(c(-lim, lim), n = 3)
            if(length(le) == 7 && le[1] == -3) {
                le = c(-3, -1.5, 0, 1.5, 3)
            } else if(! 0 %in% le) {
                le = c(le[1], le[1]/2, 0, le[length(le)]/2, le[length(le)])
            }
            legend_breaks = le
        }
    }
    if(!identical(legend_breaks, NA)) {
        heatmap_legend_param$at = legend_breaks
    }
    if(!identical(legend_labels, NA)) {
        heatmap_legend_param$labels = legend_labels
    }
    ht_param$heatmap_legend_param = heatmap_legend_param

    if(identical(annotation_colors, NA)) {
        annotation_colors = list()
    }
    if(!identical(annotation_col, NA)) {
        acn = rownames(annotation_col)
        mcn = colnames(mat)
        if(!is.null(acn)) {
            if(acn[1] %in% mcn) {
                if(length(union(acn, mcn)) == length(mcn)) {
                    if(!identical(acn, mcn)) {
                        warning_wrap("Column annotation has different order from matrix columns. Adjust the column annotation based on column names of the matrix.")
                    }
                    annotation_col = annotation_col[mcn, , drop = FALSE]
                }
            }
        }
        for(nm in colnames(annotation_col)) {
            if(nm %in% names(annotation_colors)) {
                if(is.null(names(annotation_colors[[nm]])) && is.numeric(annotation_col[, nm])) {
                    foo_x = annotation_col[, nm]
                    foo_n_col = length(annotation_colors[[nm]])
                    annotation_colors[[nm]] = colorRamp2(seq(min(foo_x), max(foo_x), length = foo_n_col), annotation_colors[[nm]])
                }
            }
        }
        ht_param$top_annotation = HeatmapAnnotation(df = annotation_col[, ncol(annotation_col):1, drop = FALSE], 
            col = annotation_colors, show_legend = annotation_legend,
            show_annotation_name = annotation_names_col, gp = gpar(col = border_color),
            annotation_name_gp = gpar(fontsize = 10, fontface = "bold"),
            simple_anno_size = unit(10, "bigpts"), gap = unit(2, "bigpts"))
    }
    if(!identical(annotation_row, NA)) {
        arn = rownames(annotation_row)
        mrn = rownames(mat)
        if(!is.null(arn)) {
            if(arn[1] %in% mrn) {
                if(length(union(arn, mrn)) == length(mrn)) {
                    if(!identical(arn, mrn)) {
                        warning_wrap("Row annotation has different order from matrix rows. Adjust the row annotation based on row names of the matrix.")
                    }
                    annotation_row = annotation_row[mrn, , drop = FALSE]
                }
            }
        }
        for(nm in colnames(annotation_row)) {
            if(nm %in% names(annotation_colors)) {
                if(is.null(names(annotation_colors[[nm]])) && is.numeric(annotation_row[, nm])) {
                    foo_x = annotation_row[, nm]
                    foo_n_col = length(annotation_colors[[nm]])
                    annotation_colors[[nm]] = colorRamp2(seq(min(foo_x), max(foo_x), length = foo_n_col), annotation_colors[[nm]])
                }
            }
        }
        ht_param$left_annotation = rowAnnotation(df = annotation_row[, ncol(annotation_row):1, drop = FALSE], 
            col = annotation_colors, show_legend = annotation_legend,
            show_annotation_name = annotation_names_row, gp = gpar(col = border_color),
            annotation_name_gp = gpar(fontsize = 10, fontface = "bold"),
            simple_anno_size = unit(10, "bigpts"), gap = unit(2, "bigpts"))
    }

    if(!identical(annotation, NA)) {
        warning_wrap("argument `annotation` is not supported in pheatmap -> Heatmap translation, skip it.")
    }

    if(identical(drop_levels, FALSE)) {
        warning_wrap("argument `drop_levels` is enfored to be TRUE, skip it.")
    }

    ht_param$show_row_names = show_rownames
    ht_param$show_column_names = show_colnames
    
    ht_param$row_names_gp = gpar(fontsize = fontsize_row)
    ht_param$column_names_gp = gpar(fontsize = fontsize_col)

    angle_col = match.arg(angle_col)[1]
    angle_col = switch(angle_col, 
                        "0" = 0,
                        "45" = 45,
                        "90" = 90,
                        "270" = 90,
                        "315" = -45)
    ht_param$column_names_rot = angle_col
    if(angle_col == 0) {
        ht_param$column_names_centered = TRUE
    }

    if(is.logical(display_numbers)) {
        if(display_numbers) {
            ht_param$layer_fun = local({
                number_format = number_format
                number_color = number_color
                fontsize_number = fontsize_number
                mat = mat
                function(j, i, x, y, w, h, fill) {
                    grid.text(sprintf(number_format, pindex(mat, i, j)), x = x, y = y, gp = gpar(col = number_color, fontsize = fontsize_number))
                }
            })
        }
    } else if(is.matrix(display_numbers)) {
        if(!identical(dim(display_numbers), dim(mat))) {
            stop_wrap("dimension of `display_numbers` should be the same as the input matrix.")
        }
        ht_param$layer_fun = local({
            number_color = number_color
            fontsize_number = fontsize_number
            mat = display_numbers
            function(j, i, x, y, w, h, fill) {
                grid.text(pindex(mat, i, j), x = x, y = y, gp = gpar(col = number_color, fontsize = fontsize_number))
            }
        })
    }
    
    if(!is.null(labels_row)) {
        ht_param$row_labels = labels_row
    }

    if(!is.null(labels_col)) {
        ht_param$column_labels = labels_col
    }

    if(!is.null(gaps_row)) {
        if(inherits(cluster_rows, c("hclust", "dendrogram"))) {
            stop_wrap("`gaps_row` should not be set when `cluster_rows` is set as a clustering object.")
        }
        if(identical(cluster_rows, TRUE)) {
            stop_wrap("`gaps_row` should not be set when `cluster_rows` is set to TRUE.")
        }
        slices = diff(c(0, gaps_row, nrow(mat)))
        ht_param$row_split = rep(seq_along(slices), times = slices)
        ht_param$row_gap = unit(4, "bigpts")
        ht_param["row_title"] = list(NULL)
    }
    if(!is.null(gaps_col)) {
        if(inherits(cluster_cols, c("hclust", "dendrogram"))) {
            stop_wrap("`gaps_col` should not be set when `cluster_cols` is set as a clustering object.")
        }
        if(identical(cluster_cols, TRUE)) {
            stop_wrap("`gaps_col` should not be set when `cluster_cols` is set to TRUE.")
        }
        slices = diff(c(0, gaps_col, ncol(mat)))
        ht_param$column_split = rep(seq_along(slices), times = slices)
        ht_param$column_gap = unit(4, "bigpts")
        ht_param["column_title"] = list(NULL)
    }

    if(!identical(clustering_callback, NA)) {
        if(!identical(ht_param$cluster_rows, FALSE)) {
            row_hclust = hclust(get_dist(mat, ht_param$clustering_distance_rows), ht_param$clustering_method_rows)
            row_hclust = clustering_callback(row_hclust, ...)
            ht_param$cluster_rows = row_hclust
        }
        if(!identical(ht_param$cluster_columns, FALSE)) {
            column_hclust = hclust(get_dist(t(mat), ht_param$clustering_distance_columns), ht_param$clustering_method_columns)
            column_hclust = clustering_callback(column_hclust, ...)
            ht_param$cluster_columns = column_hclust
        }
    }

    ### default from pheatmap
    ht_param$name = name
    ht_param$row_dend_reorder = FALSE
    ht_param$column_dend_reorder = FALSE

    if(!identical(main, NA)) {
        ht_param$column_title = main
        ht_param$column_title_gp = gpar(fontface = "bold", fontsize = 1.3*fontsize)
    }
    ht_param = c(ht_param, list(...))
    ht = do.call(Heatmap, ht_param)
    attr(ht, "translate_from") = "pheatmap"
    ht
}

# == title
# Compare heatmaps between pheatmap::pheatmap() and ComplexHeatmap::pheatmap()
#
# == param
# -... The same set of arguments passed to ``pheatmap::pheatmap`` and ``ComplexHeatmap::pheatmap``.
#
# == details
# The function plots two heatmaps, one by ``pheatmap::pheatmap`` and one by ``ComplexHeatmap::pheatmap``.
# Users can see the difference between the two implementations.
#
# == example
# mat = matrix(rnorm(100), 10)
# compare_pheatmap(mat)
compare_pheatmap = function(...) {
    if(!requireNamespace("pheatmap")) {
        stop_wrap("pheatmap package should be installed.")
    }
    p1 = pheatmap::pheatmap(..., silent = TRUE)$gtable
    p2 = grid.grabExpr(draw(pheatmap(...)))
    grid.newpage()
    pushViewport(viewport(x = 0, width = 0.5, y = 0, height = unit(1, "npc") - unit(1, "cm"), just = c("left", "bottom")))
    grid.draw(p1)
    popViewport()
    pushViewport(viewport(x = 0, width = 0.5, y = 1, height = unit(1, "cm"), just = c("left", "top")))
    grid.text("pheatmap::pheatmap()")
    popViewport()
    pushViewport(viewport(x = 0.5, width = 0.5, y = 0, height = unit(1, "npc") - unit(1, "cm"), just = c("left", "bottom")))
    grid.draw(p2)
    popViewport()
    pushViewport(viewport(x = 0.5, width = 0.5, y = 1, height = unit(1, "cm"), just = c("left", "top")))
    grid.text("ComplexHeatmap::pheatmap()")
    popViewport()
}

