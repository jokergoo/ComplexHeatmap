
# Translate stats::heatmap to ComplexHeatmap::Heatmap
#
# == alias
# stats_heatmap
#
# == param
# -x The input matrix.
# -col A vector of colors.
# -Rowv The same as in `stats::heatmap`.
# -Colv The same as in `stats::heatmap`.
# -distfun The same as in `stats::heatmap`.
# -hclustfun The same as in `stats::heatmap`.
# -reorderfun The same as in `stats::heatmap`.
# -add.expr Ignored.
# -symm Ignored.
# -revC Ignored.
# -scale The same as in `stats::heatmap`.
# -na.rm Ignored.
# -margins Ignored.
# -ColSideColors The same as in `stats::heatmap`.
# -RowSideColors The same as in `stats::heatmap`.
# -cexRow The same as in `stats::heatmap`.
# -cexCol The same as in `stats::heatmap`.
# -labRow The same as in `stats::heatmap`.
# -labCol The same as in `stats::heatmap`.
# -main The same as in `stats::heatmap`.
# -xlab The same as in `stats::heatmap`.
# -ylab The same as in `stats::heatmap`.
# -keep.dendro Ignored.
# -verbose Ignored.
# -... Other arguments passed to `Heatmap`.
# -run_draw Whether to run ``draw()`` function to the heatmap object.
#
# == details
# This function aims to execute ``stats::heatmap`` code purely with ComplexHeatmap.
#
# == value
# A `Heatmap-class` object.
#
# == seealso
# `compare_heatmap` that compares heatmaps between ``stats::heatmap()`` and ``ComplexHeatmap::heatmap()``.
heatmap = function(x, 
	col = hcl.colors(12, "YlOrRd", rev = TRUE),
	Rowv = NULL, 
	Colv = NULL,
    distfun = dist, 
    hclustfun = hclust,
    reorderfun = function(d, w) reorder(d, w),
    add.expr, 
    symm = FALSE, 
    revC = identical(Colv, "Rowv"),
    scale = c("row", "column", "none"), 
    na.rm = TRUE,
    margins = c(5, 5), 
    ColSideColors, 
    RowSideColors,
    cexRow = 0.6, 
    cexCol = 0.6,
    labRow = NULL, 
    labCol = NULL, 
    main = NULL,
    xlab = NULL, 
    ylab = NULL,
    keep.dendro = FALSE, 
    verbose = getOption("verbose"), 
    ...,
    run_draw = FALSE) {

	if(is.data.frame(x)) {
        warning_wrap("The input is a data frame, convert it to the matrix.")
        mat = as.matrix(x)
    } else {
    	mat = x
    }

    nr = nrow(mat)
    nc = ncol(mat)

    ht_param = list()

    # Rowv can be 1. NA, 2. a dendrogram object, 3. a vector, 4. a logical value, 5. NULL
    if(identical(Rowv, NA)) {
    	ht_param$cluster_rows = FALSE

    	mat = mat[nr:1, , drop = FALSE]
    	if(!missing(RowSideColors)) {
    		RowSideColors = rev(RowSideColors)
    	}
        if(is.null(rownames(mat))) labRow = seq_len(nr)
        if(!is.null(labRow)) {
            labRow = rev(labRow)
        }
    } else {
    	if(is.null(Rowv)) {
    		row_dend = as.dendrogram(hclustfun(distfun(mat)))
    		row_dend = reorderfun(row_dend, -rowMeans(mat))
    	} else if(inherits(Rowv, c("dendrogram", "hclust"))) {
    		row_dend = as.dendrogram(Rowv)
    	} else if(is.vector(Rowv)) {
    		if(length(Rowv) == nr) {
    			row_dend = as.dendrogram(hclustfun(distfun(mat)))
    			row_dend = reorderfun(row_dend, Rowv)
    		} else if(length(Rowv) == 1 && is.logical(Rowv)) {
    			if(Rowv) {
    				row_dend = as.dendrogram(hclustfun(distfun(mat)))
    				row_dend = reorderfun(row_dend, -rowMeans(mat))
    			} else {
    				row_dend = as.dendrogram(hclustfun(distfun(mat)))
    				row_dend = rev(row_dend)
    			}
    		} else {
    			stop_wrap("Wrong value for 'Rowv'.")
    		}
    	}

    	ht_param$cluster_rows = row_dend
    }

    if(identical(Colv, NA)) {
    	ht_param$cluster_columns = FALSE
    } else {
    	if(is.null(Colv)) {
    		column_dend = as.dendrogram(hclustfun(distfun(t(mat))))
    		column_dend = reorderfun(column_dend, colMeans(mat))
    	} else if(inherits(Colv, c("dendrogram", "hclust"))) {
    		column_dend = as.dendrogram(Colv)
    	} else if(is.vector(Colv)) {
    		if(length(Colv) == nc) {
    			column_dend = as.dendrogram(hclustfun(distfun(t(mat))))
    			column_dend = reorderfun(column_dend, Colv)
    		} else if(length(Colv) == 1 && is.logical(Colv)) {
    			if(Colv) {
    				column_dend = as.dendrogram(hclustfun(distfun(t(mat))))
    				column_dend = reorderfun(column_dend, -colMeans(mat))
    			} else {
    				column_dend = as.dendrogram(hclustfun(distfun(t(mat))))
    			}
    		} else {
    			stop_wrap("Wrong value for 'Colv'.")
    		}
    	}

    	ht_param$cluster_columns = column_dend
    }

    ht_param$row_dend_width = unit(2, "cm")
    ht_param$column_dend_height = unit(2, "cm")

    scale = match.arg(scale)[1]
    if("row" %in% scale) {

        if(any(is.na(mat))) {
            mat = (mat - rowMeans(mat, na.rm = TRUE))/rowSds(mat, na.rm = TRUE)
        } else {
            mat = t(scale(t(mat)))
        }
        message_wrap("Note, in 'heatmap()', when rows are scaled, the row dendrogram is still calculated from the original matrix (not from the scaled matrix).")
    } else if("column" %in% scale) {
        if(any(is.na(mat))) {
            mat = t((t(mat) - colMeans(mat, na.rm = TRUE))/colSds(mat, na.rm = TRUE))
        } else {
            mat = scale(mat)
        }
        message_wrap("Note, in 'heatmap()', when columns are scaled, the column dendrogram is still calculated from the original matrix (not from the scaled matrix).")
    }

    ht_param$matrix = mat

    # if color is a color mapping function
    if(is.function(col)) {
        ht_param$col = col
    } else {
        n_col = length(col)
        if(identical(scale, "row") || identical(scale, "column")) {
            lim = max(abs(mat), na.rm = TRUE)
            ht_param$col = colorRamp2(seq(-lim, lim, length.out = n_col), col)
        } else {
            ht_param$col = colorRamp2(seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE), length.out = n_col), col)
        } 
    }
    
    if(!missing(ColSideColors)) {
    	ht_param$top_annotation = HeatmapAnnotation(column = ColSideColors,
    		col = list(column = structure(unique(ColSideColors), names = unique(ColSideColors))),
    		show_legend = FALSE, show_annotation_name = FALSE)
    }
    if(!missing(RowSideColors)) {
    	ht_param$left_annotation = rowAnnotation(row = RowSideColors,
    		col = list(row = structure(unique(RowSideColors), names = unique(RowSideColors))),
    		show_legend = FALSE, show_annotation_name = FALSE)
    }

    if(!is.null(labRow)) {
    	ht_param$row_labels = labRow
    } else if(is.null(rownames(mat))) {
    	ht_param$row_labels = seq_len(nr)
    } else {
    	ht_param$row_labels = rownames(mat)
    }

    if(!is.null(labCol)) {
    	ht_param$column_labels = labCol
    } else if(is.null(colnames(mat))) {
    	ht_param$column_labels = seq_len(nc)
    } else {
    	ht_param$column_labels = colnames(mat)
    }

    ht_param$row_names_gp = gpar(fontsize = cexRow*12)
    ht_param$column_names_gp = gpar(fontsize = cexCol*12)

    if(!is.null(main)) {
    	ht_param$column_title = main
    }
    if(!is.null(xlab)) {
    	if(is.null(ht_param$column_labels)) {
    		ht_param$bottom_annotation = HeatmapAnnotation(xlab = anno_block(labels = xlab, gp = gpar(col = NA)))
    	} else {
    		ht_param$bottom_annotation = HeatmapAnnotation(
    			colnames = anno_text(ht_param$column_labels, gp = ht_param$column_names_gp),
    			xlab = anno_block(labels = xlab, gp = gpar(col = NA))
    		)
    		ht_param$show_column_names = FALSE
    	}
    }
    if(!is.null(ylab)) {
    	if(is.null(ht_param$row_labels)) {
    		ht_param$right_annotation = rowAnnotation(ylab = anno_block(labels = ylab, gp = gpar(col = NA)))
    	} else {
    		ht_param$right_annotation = rowAnnotation(
    			rownames = anno_text(ht_param$row_labels, gp = ht_param$row_names_gp),
    			ylab = anno_block(labels = ylab, gp = gpar(col = NA))
    		)
    		ht_param$show_row_names = FALSE
    	}
    }

    heatmap_legend_param = list(title = "")
    ht_param$heatmap_legend_param = heatmap_legend_param

    if(!missing(add.expr)) {
    	warning_wrap("argument `add.expr` is not supported in heatmap -> Heatmap translation, skip it.")
    }

    ht_param = c(ht_param, list(...))
    ht = do.call(Heatmap, ht_param)
    attr(ht, "translate_from") = "heatmap"

    if(run_draw) {
        draw(ht)
    } else {
        ht
    }
}

# == title
# Compare heatmaps between stats::heatmap() and ComplexHeatmap::heatmap()
#
# == param
# -... The same set of arguments passed to ``stats::heatmap`` and ``ComplexHeatmap::heatmap``.
#
# == details
# The function plots two heatmaps, one by ``stats::heatmap`` and one by ``ComplexHeatmap::heatmap``.
# Users can see the difference between the two implementations.
#
# == example
# mat = matrix(rnorm(100), 10)
# compare_heatmap(mat)
compare_heatmap = function(...) {
    p1 = gridGraphics::echoGrob(function() stats::heatmap(...))
    p2 = grid.grabExpr(draw(heatmap(...)))
    grid.newpage()
    pushViewport(viewport(x = 0, width = 0.5, y = 0, height = unit(1, "npc") - unit(1, "cm"), just = c("left", "bottom")))
    grid.draw(p1)
    popViewport()
    pushViewport(viewport(x = 0, width = 0.5, y = 1, height = unit(1, "cm"), just = c("left", "top")))
    grid.text("stats::heatmap()")
    popViewport()
    pushViewport(viewport(x = 0.5, width = 0.5, y = 0, height = unit(1, "npc") - unit(1, "cm"), just = c("left", "bottom")))
    grid.draw(p2)
    popViewport()
    pushViewport(viewport(x = 0.5, width = 0.5, y = 1, height = unit(1, "cm"), just = c("left", "top")))
    grid.text("ComplexHeatmap::heatmap()")
    popViewport()
}


# Translate gplots::heatmap.2 to ComplexHeatmap::Heatmap
#
# == param
# -x The input matrix.
# -Rowv The same as in `gplots::heatmap.2`.
# -Colv The same as in `gplots::heatmap.2`.
# -distfun The same as in `gplots::heatmap.2`.
# -hclustfun The same as in `gplots::heatmap.2`.
# -dendrogram The same as in `gplots::heatmap.2`.
# -reorderfun The same as in `gplots::heatmap.2`.
# -symm Ignored.
# -scale The same as in `gplots::heatmap.2`.
# -na.rm Ignored.
# -revC Ignored.
# -add.expr Ignored.
# -breaks The same as in `gplots::heatmap.2`.
# -symbreaks Ignored.
# -col The same as in `gplots::heatmap.2`.
# -colsep Ignored.
# -rowsep Ignored.
# -sepcolor Ignored.
# -sepwidth Ignored.
# -cellnote The same as in `gplots::heatmap.2`.
# -notecex The same as in `gplots::heatmap.2`.
# -notecol The same as in `gplots::heatmap.2`.
# -na.color The same as in `gplots::heatmap.2`.
# -trace 'both' is reset to 'column', others are the same as in `gplots::heatmap.2`.
# -tracecol The same as in `gplots::heatmap.2`.
# -hline The same as in `gplots::heatmap.2`.
# -vline The same as in `gplots::heatmap.2`.
# -linecol The same as in `gplots::heatmap.2`.
# -margins Ignored.
# -ColSideColors The same as in `gplots::heatmap.2`.
# -RowSideColors The same as in `gplots::heatmap.2`.
# -cexRow The same as in `gplots::heatmap.2`.
# -cexCol The same as in `gplots::heatmap.2`.
# -labRow The same as in `gplots::heatmap.2`.
# -labCol The same as in `gplots::heatmap.2`.
# -srtRow Ignored.
# -srtCol Ignored.
# -adjRow Ignored.
# -adjCol Ignored.
# -offsetRow Ignored.
# -offsetCol Ignored.
# -colRow The same as in `gplots::heatmap.2`.
# -colCol The same as in `gplots::heatmap.2`.
# -key Always TRUE.
# -keysize Ignored.
# -density.info The same as in `gplots::heatmap.2`.
# -denscol The same as in `gplots::heatmap.2`.
# -symkey Ignored.
# -densadj The same as in `gplots::heatmap.2`.
# -key.title Always "Color Key".
# -key.xlab Always "Value".
# -key.ylab "Count" or "Density", depends on the value of ``density.info``.
# -key.xtickfun Ignored.
# -key.ytickfun Ignored.
# -key.par Ignored.
# -main The same as in `gplots::heatmap.2`.
# -xlab The same as in `gplots::heatmap.2`.
# -ylab The same as in `gplots::heatmap.2`.
# -lmat Ignored.
# -lhei Ignored.
# -lwid Ignored.
# -extrafun Ignored.
# -... Other arguments passed to `Heatmap`.
# -run_draw Whether to run ``draw()`` function to the heatmap object.
#
# == details
# This function aims to execute ``gplots::heatmap.2`` code purely with ComplexHeatmap.
#
# == value
# A `Heatmap-class` object.
#
# == seealso
# `compare_heatmap.2` that compares heatmaps between ``gplots::heatmap.2()`` and ``ComplexHeatmap::heatmap.2()``.
heatmap.2 = function(x,

    # dendrogram control
    Rowv = TRUE,
    Colv = TRUE,
    distfun = dist,
    hclustfun = hclust,
    dendrogram = c("both","row","column","none"),
    reorderfun = function(d, w) reorder(d, w),
    symm = FALSE,

    # data scaling
    scale = c("none","row", "column"),
    na.rm = TRUE,

    # image plot
    revC = identical(Colv, "Rowv"),
    add.expr,

    # mapping data to colors
    breaks,
    symbreaks = any(x < 0, na.rm = TRUE) || scale != "none",

    # colors
    col = "heat.colors",

    # block sepration
    colsep,
    rowsep,
    sepcolor = "white",
    sepwidth = c(0.05, 0.05),

    # cell labeling
    cellnote,
    notecex = 0.6,
    notecol = "cyan",
    na.color = par("bg"),

    # level trace
    trace = c("column", "row", "both", "none"),
    tracecol = "cyan",
    hline = median(breaks),
    vline = median(breaks),
    linecol = tracecol,

    # Row/Column Labeling
    margins = c(5, 5),
    ColSideColors,
    RowSideColors,
    cexRow = 0.6,
    cexCol = 0.6,
    labRow = NULL,
    labCol = NULL,
    srtRow = NULL,
    srtCol = NULL,
    adjRow = c(0,NA),
    adjCol = c(NA,0),
    offsetRow = 0.5,
    offsetCol = 0.5,
    colRow = NULL,
    colCol = NULL,

    # color key + density info
    key = TRUE,
    keysize = 1.5,
    density.info = c("histogram", "density", "none"),
    denscol = tracecol,
    symkey = any(x < 0, na.rm = TRUE) || symbreaks,
    densadj = 0.25,
    key.title = NULL,
    key.xlab = NULL,
    key.ylab = NULL,
    key.xtickfun = NULL,
    key.ytickfun = NULL,
    key.par = list(),

    # plot labels
    main = NULL,
    xlab = NULL,
    ylab = NULL,

    # plot layout
    lmat = NULL,
    lhei = NULL,
    lwid = NULL,

    # extras
    extrafun = NULL,
    ...,
    run_draw = FALSE
    ) {

    if(is.data.frame(x)) {
        warning_wrap("The input is a data frame, convert it to the matrix.")
        mat = as.matrix(x)
    } else {
        mat = x
    }

    nr = nrow(mat)
    nc = ncol(mat)

    ht_param = list()

    if(identical(Rowv, FALSE) && identical(symm, TRUE)) {
        Colv = FALSE
    }

    # Rowv can be 1. NA, 2. a dendrogram object, 3. a vector, 4. a logical value, 5. NULL
    if(identical(Rowv, NA) || identical(Rowv, NULL) || identical(Rowv, FALSE)) {
        ht_param$cluster_rows = FALSE
    } else {
        if(inherits(Rowv, c("dendrogram", "hclust"))) {
            row_dend = as.dendrogram(Rowv)
        } else if(is.vector(Rowv)) {
            if(length(Rowv) == nr) {
                row_dend = as.dendrogram(hclustfun(distfun(mat)))
                row_dend = reorderfun(row_dend, Rowv)
            } else if(length(Rowv) == 1 && is.logical(Rowv)) {
                if(Rowv) {
                    row_dend = as.dendrogram(hclustfun(distfun(mat)))
                    row_dend = reorderfun(row_dend, -rowMeans(mat))
                }
            } else {
                stop_wrap("Wrong value for 'Rowv'.")
            }
        }

        ht_param$cluster_rows = row_dend
    }

    if(identical(Colv, NA) || identical(Colv, NULL) || identical(Colv, FALSE)) {
        ht_param$cluster_columns = FALSE
    } else {
        if(inherits(Colv, c("dendrogram", "hclust"))) {
            column_dend = as.dendrogram(Colv)
        } else if(is.vector(Colv)) {
            if(length(Colv) == nc) {
                column_dend = as.dendrogram(hclustfun(distfun(t(mat))))
                column_dend = reorderfun(column_dend, Colv)
            } else if(length(Colv) == 1 && is.logical(Colv)) {
                if(Colv) {
                    column_dend = as.dendrogram(hclustfun(distfun(t(mat))))
                    column_dend = reorderfun(column_dend, colMeans(mat))
                }
            } else {
                stop_wrap("Wrong value for 'Colv'.")
            }
        }

        ht_param$cluster_columns = column_dend
    }

    dendrogram = match.arg(dendrogram)[1]
    if("both" %in% dendrogram) {
        ht_param$show_row_dend = TRUE
        ht_param$show_column_dend = TRUE
    } else if("row" %in% dendrogram) {
        ht_param$show_row_dend = TRUE
        ht_param$show_column_dend = FALSE
    } else if("column" %in% dendrogram) {
        ht_param$show_row_dend = FALSE
        ht_param$show_column_dend = TRUE
    } else if("none" %in% dendrogram) {
        ht_param$show_row_dend = FALSE
        ht_param$show_column_dend = FALSE
    }

    ht_param$row_dend_width = unit(4, "cm")
    ht_param$column_dend_height = unit(3, "cm")

    
    scale = match.arg(scale)[1]
    if("row" %in% scale) {
        if(any(is.na(mat))) {
            mat = (mat - rowMeans(mat, na.rm = TRUE))/rowSds(mat, na.rm = TRUE)
        } else {
            mat = t(scale(t(mat)))
        }
        message_wrap("Note, in 'heatmap.2()', when rows are scaled, the row dendrogram is still calculated from the original matrix (not from the scaled matrix).")
    } else if("column" %in% scale) {
        if(any(is.na(mat))) {
            mat = t((t(mat) - colMeans(mat, na.rm = TRUE))/colSds(mat, na.rm = TRUE))
        } else {
            mat = scale(mat)
        }
        message_wrap("Note, in 'heatmap.2()', when columns are scaled, the column dendrogram is still calculated from the original matrix (not from the scaled matrix).")
    }

    ht_param$matrix = mat

    ##### how to process `col` is directly from gplots::heatmap.2 ####
    if(is.character(col) && length(col) == 1) {
        col <- get(col, mode="function")
    }
    if(missing(breaks) || is.null(breaks) || length(breaks)<1 ) {
      if( missing(col) ||  is.function(col) )
        breaks <- 16
      else
        breaks <- length(col)+1
    }

    if(length(breaks)==1)
    {
      if(!symbreaks)
        breaks <- seq( min(mat, na.rm=na.rm), max(mat,na.rm=na.rm), length.out = breaks)
      else
        {
          extreme <- max(abs(mat), na.rm=TRUE)
          breaks <- seq( -extreme, extreme, length.out = breaks )
        }
    }

    nbr <- length(breaks)
    ncol <- length(breaks)-1

    if(inherits(col, "function")) col =col(ncol)
    #### until here ###
    
    n_col = ncol

    if(exists("extreme")) {
        lim = max(abs(mat), na.rm = TRUE)
        ht_param$col = colorRamp2(seq(-lim, lim, length.out = n_col), col)
    } else {
        ht_param$col = colorRamp2(seq(min(mat, na.rm = TRUE), max(mat, na.rm = TRUE), length.out = n_col), col)
    } 
    

    if(!missing(colsep)) {
        warning_wrap("argument `colsep` is not supported in heatmap.2 -> Heatmap translation, skip it. Suggest to use `column_split` argument in Heatmap() which can be directly used here.")
    }
    if(!missing(rowsep)) {
        warning_wrap("argument `rowsep` is not supported in heatmap.2 -> Heatmap translation, skip it. Suggest to use `row_split` argument in Heatmap() which can be directly used here.")
    }

    if(!missing(cellnote)) {
        ht_param$layer_fun = function(j, i, x, y, w, h, fill) {
            grid.text(pindex(cellnote, i, j), x, y, gp = gpar(cex = notecex, col = notecol))
        }
    }
    ht_param$na_col = na.color
    
    if(!missing(ColSideColors)) {
        ht_param$top_annotation = HeatmapAnnotation(column = ColSideColors,
            col = list(column = structure(unique(ColSideColors), names = unique(ColSideColors))),
            show_legend = FALSE, show_annotation_name = FALSE)
    }
    if(!missing(RowSideColors)) {
        ht_param$left_annotation = rowAnnotation(row = RowSideColors,
            col = list(row = structure(unique(RowSideColors), names = unique(RowSideColors))),
            show_legend = FALSE, show_annotation_name = FALSE)
    }

    if(identical(ht_param$cluster_rows, FALSE) || dendrogram %in% c("none", "column")) {
        if(is.null(ht_param$left_annotation)) {
            ht_param$left_annotation = rowAnnotation(foo1 = anno_empty(width = unit(4, "cm"), border = FALSE))
        } else {
            ht_param$left_annotation = c(ht_param$left_annotation, rowAnnotation(foo1 = anno_empty(width = unit(3.5, "cm"))))
        }
    }

    if(identical(ht_param$cluster_columns, FALSE) || dendrogram %in% c("none", "row")) {
        if(is.null(ht_param$top_annotation)) {
            ht_param$top_annotation = HeatmapAnnotation(foo2 = anno_empty(height = unit(3, "cm"), border = FALSE))
        } else {
            ht_param$top_annotation = c(ht_param$top_annotation, HeatmapAnnotation(foo2 = anno_empty(height = unit(3.5, "cm"))))
        }
    }

    if(!is.null(labRow)) {
        ht_param$row_labels = labRow
    } else if(is.null(rownames(mat))) {
        ht_param$row_labels = seq_len(nr)
    } else {
        ht_param$row_labels = rownames(mat)
    }

    if(!is.null(labCol)) {
        ht_param$column_labels = labCol
    } else if(is.null(colnames(mat))) {
        ht_param$column_labels = seq_len(nc)
    } else {
        ht_param$column_labels = colnames(mat)
    }

    if(is.null(colRow)) colRow = "black"
    ht_param$row_names_gp = gpar(fontsize = 12*cexRow, col = colRow)
    if(is.null(colCol)) colCol = "black"
    ht_param$column_names_gp = gpar(fontsize = 12*cexCol, col = colCol)

    if(!is.null(srtRow)) {
        ht_param$row_names_rot = srtRow
    } else {
        ht_param$row_names_rot = 0
    }
    if(!is.null(srtCol)) {
        ht_param$column_names_rot = srtCol
    } else {
        ht_param$column_names_rot = 90
    }

    if(!is.null(main)) {
        ht_param$column_title = main
    }
    if(!is.null(xlab)) {
        if(is.null(ht_param$column_labels)) {
            ht_param$bottom_annotation = HeatmapAnnotation(xlab = anno_block(labels = xlab, gp = gpar(col = NA)))
        } else {
            ht_param$bottom_annotation = HeatmapAnnotation(
                colnames = anno_text(ht_param$column_labels, gp = ht_param$column_names_gp, rot = ht_param$column_names_rot),
                xlab = anno_block(labels = xlab, gp = gpar(col = NA))
            )
            ht_param$show_column_names = FALSE
        }
    }
    if(!is.null(ylab)) {
        if(is.null(ht_param$row_labels)) {
            ht_param$right_annotation = rowAnnotation(ylab = anno_block(labels = ylab, gp = gpar(col = NA)))
        } else {
            ht_param$right_annotation = rowAnnotation(
                rownames = anno_text(ht_param$row_labels, gp = ht_param$row_names_gp, rot = ht_param$row_names_rot),
                ylab = anno_block(labels = ylab, gp = gpar(col = NA))
            )
            ht_param$show_row_names = FALSE
        }
    }

    ht_param$show_heatmap_legend = FALSE

    trace = match.arg(trace)[1]
    min = breaks[1]
    max = breaks[length(breaks)]
    rg = max - min

    layer_fun = NULL
    if(trace == "both") {
        warning_wrap("trace = 'both' is not supported, change to trace = 'column'.")
        trace = "column"
    }
    if(trace == "column") {
        layer_fun = function(j, i, x, y, w, h, fill) {
            ind_mat = restore_matrix(j, i, x, y)

            ind = ind_mat[1, ]
            grid.segments(x[ind], unit(0, "npc"), x[ind], unit(1, "npc"), gp = gpar(col = linecol, lty = 2))
            for(ki in seq_len(ncol(ind_mat))) {
                ind = ind_mat[, ki]
                offset = (mat[i[ind], j[ind[1]]] - min)/(max - min)*w[ind]
                pos_x = rep(x[ind] - w[ind]*0.5 + offset, each = 2)
                pos_y = rep(y[ind] + h[ind]*0.5, each = 2)
                pos_y[seq_along(pos_y) %% 2 == 0] = y[ind] - h[ind]*0.5
                grid.lines(pos_x, pos_y, gp = gpar(col = tracecol))
            }
        }
    } else if(trace == "row") {
        layer_fun = function(j, i, x, y, w, h, fill) {
            ind_mat = restore_matrix(j, i, x, y)

            ind = ind_mat[, 1]
            grid.segments(unit(0, "npc"), y[ind], unit(1, "npc"), y[ind], gp = gpar(col = linecol, lty = 2))
            for(ki in seq_len(nrow(ind_mat))) {
                ind = ind_mat[ki, ]
                offset = (mat[i[ind[1]], j[ind]] - min)/(max - min)*h[ind]
                pos_x = rep(x[ind] - w[ind]*0.5, each = 2)
                pos_x[seq_along(pos_x) %% 2 == 0] = x[ind] + w[ind]*0.5
                pos_y = rep(y[ind] - h[ind]*0.5 + offset, each = 2)
                grid.lines(pos_x, pos_y, gp = gpar(col = tracecol))
            }
        }
    }
    if(!is.null(ht_param$layer_fun) && trace %in% c("row", "column")) {
        fun1 = ht_param$layer_fun
        fun2 = layer_fun
        fun3 = function(j, i, x, y, w, h, fill) {
            fun1(j, i, x, y, w, h, fill)
            fun2(j, i, x, y, w, h, fill)
        }
        layer_fun = fun3
        ht_param$layer_fun = layer_fun
    } else if(is.null(ht_param$layer_fun) && trace %in% c("row", "column")) {
        ht_param$layer_fun = layer_fun
    }

    random_str = paste(sample(c(letters, LETTERS, 0:9), 8), collapse = "")
    ht_param$name = paste0("heatmap.2_", random_str)

    density.info = match.arg(density.info)[1]
    if(is.null(key.xlab)) key.xlab = "Value"
    if(is.null(key.ylab)) {
        if(density.info == "histogram") key.ylab = "Count"
        if(density.info == "density") key.ylab = "Density"
    }

    if(density.info == "none") {
        post_fun = NULL
    } else {
        post_fun = function(ht) {
            decorate_heatmap_body(paste0("heatmap.2_", random_str), {
                pushViewport(viewport(unit(0, "npc"), unit(1, "npc"), width = unit(4, "cm"), height = unit(3, "cm"), just = c("right", "bottom")))

                left_width = unit(1, "cm")
                bottom_height = unit(1, "cm")
                top_height = unit(0.5, "cm")

                if(density.info == "histogram") {
                    tb = hist(mat, breaks = breaks, plot = FALSE)
                    x_at = pretty(range(tb$breaks), n = 3)
                    y_at = pretty(range(tb$counts), n = 3)
                    x_range = range(tb$breaks)
                    y_range = range(tb$counts); y_range[2] = y_range[2] + (y_range[2] - y_range[1])*0.05
                } else if(density.info == "density") {
                    den = density(mat, na.rm = TRUE, from = min(breaks), to = max(breaks), adjust = densadj)
                    den_x = den$x
                    den_y = den$y
                    x_range = range(breaks)
                    l = den_x >= x_range[1] & den_x <= x_range[2]
                    den_x = den_x[l]
                    den_y = den_y[l]
                    x_at = pretty(range(den_x), n = 3)
                    y_at = pretty(range(den_y), n = 3)
                    y_range = range(den_y); y_range[2] = y_range[2] + (y_range[2] - y_range[1])*0.05
                }

                x_at = x_at[x_at >= x_range[1] & x_at <= x_range[2]]
                y_at = y_at[y_at >= y_range[1] & y_at <= y_range[2]]
                pushViewport(viewport(x = left_width, y = bottom_height, 
                    width = unit(1, "npc") - left_width - unit(2, "mm"), height = unit(1, "npc") - bottom_height- top_height,
                    just = c("left", "bottom"), xscale = x_range, yscale = y_range))

                x = seq(min(breaks), max(breaks), length.out = 101)
                grid.rect(x = x[1:100], width = (x_range[2] - x_range[1])/100, default.units = "native", just = "left",
                    gp = gpar(fill = ht_param$col(x + (x_range[2] - x_range[1])/100*0.5), col = NA))

                if(density.info == "histogram") {
                    x = rep(tb$breaks, each = 2); x = x[-c(1, length(x))]
                    y = rep(tb$counts, each = 2)
                } else if(density.info == "density") {
                    x = den_x
                    y = den_y
                }
                grid.lines(x, y, default.units = "native", gp = gpar(col = denscol))
                if(trace == "column") {
                    grid.lines(c(vline, vline), unit(c(0, 1), "npc"), default.units = "native", gp = gpar(col = linecol, lty = 2))
                } else if(trace == "row") {
                    grid.lines(unit(c(0, 1), "npc"), c(hline, hline), default.units = "native", gp = gpar(col = linecol, lty = 2))
                }
                grid.rect(gp = gpar(fill = "transparent"))
                grid.xaxis(at = x_at, gp = gpar(fontsize = 8))
                grid.text(key.xlab, y = unit(0, "npc") - unit(8, "mm"), gp = gpar(fontsize = 8))
                grid.yaxis(at = y_at, gp = gpar(fontsize = 8))
                grid.text(key.ylab, x = unit(0, "npc") - unit(8, "mm"), gp = gpar(fontsize = 8), rot = 90)
                grid.text("Color Key", y = unit(1, "npc") + top_height*0.5, gp = gpar(fontface = "bold", fontsize = 10))
                popViewport()

                popViewport()
            })
        }
    }
    ht_param$post_fun = post_fun
    
    if(!missing(add.expr)) {
        warning_wrap("argument `add.expr` is not supported in heatmap.2 -> Heatmap translation, skip it.")
    }

    if(!is.null(lmat)) {
        warning_wrap("argument `lmat` is not supported in heatmap.2 -> Heatmap translation, skip it.")
    }
    if(!is.null(lhei)) {
        warning_wrap("argument `lhei` is not supported in heatmap.2 -> Heatmap translation, skip it.")
    }
    if(!is.null(lwid)) {
        warning_wrap("argument `lwid` is not supported in heatmap.2 -> Heatmap translation, skip it.")
    }
    if(!is.null(extrafun)) {
        warning_wrap("argument `extrafun` is not supported in heatmap.2 -> Heatmap translation, skip it.")
    }

    ht_param = c(ht_param, list(...))
    ht = do.call(Heatmap, ht_param)
    attr(ht, "translate_from") = "heatmap"

    if(run_draw) {
        draw(ht)
    } else {
        ht
    }
}

# == title
# Compare heatmaps between gplots::heatmap.2() and ComplexHeatmap::heatmap()
#
# == param
# -... The same set of arguments passed to ``gplots::heatmap.2`` and ``ComplexHeatmap::heatmap.2``.
#
# == details
# The function plots two heatmaps, one by ``gplots::heatmap.2`` and one by ``ComplexHeatmap::heatmap.2``.
# Users can see the difference between the two implementations.
#
# == example
# mat = matrix(rnorm(100), 10)
# compare_heatmap.2(mat)
compare_heatmap.2 = function(...) {
    p1 = gridGraphics::echoGrob(function() gplots::heatmap.2(...))
    p2 = grid.grabExpr(draw(heatmap.2(...)))
    grid.newpage()
    pushViewport(viewport(x = 0, width = 0.5, y = 0, height = unit(1, "npc") - unit(1, "cm"), just = c("left", "bottom")))
    grid.draw(p1)
    popViewport()
    pushViewport(viewport(x = 0, width = 0.5, y = 1, height = unit(1, "cm"), just = c("left", "top")))
    grid.text("gplots::heatmap.2()")
    popViewport()
    pushViewport(viewport(x = 0.5, width = 0.5, y = 0, height = unit(1, "npc") - unit(1, "cm"), just = c("left", "bottom")))
    grid.draw(p2)
    popViewport()
    pushViewport(viewport(x = 0.5, width = 0.5, y = 1, height = unit(1, "cm"), just = c("left", "top")))
    grid.text("ComplexHeatmap::heatmap.2()")
    popViewport()
}
