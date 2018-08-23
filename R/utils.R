
# environment that contains global variables
INDEX_ENV = new.env()

INDEX_ENV$I_FIGURE = 0
INDEX_ENV$I_HEATMAP = 0
INDEX_ENV$I_ANNOTATION = 0
INDEX_ENV$I_ROW_ANNOTATION = 0
INDEX_ENV$I_COLOR_MAPPING = 0

get_figure_index = function() {
    INDEX_ENV$I_FIGURE
}

increase_figure_index = function() {
    INDEX_ENV$I_FIGURE = INDEX_ENV$I_FIGURE + 1
}

get_heatmap_index = function() {
	INDEX_ENV$I_HEATMAP
}

increase_heatmap_index = function() {
	INDEX_ENV$I_HEATMAP = INDEX_ENV$I_HEATMAP + 1
}

get_annotation_index = function() {
	INDEX_ENV$I_ANNOTATION
}

increase_annotation_index = function() {
	INDEX_ENV$I_ANNOTATION = INDEX_ENV$I_ANNOTATION + 1
}

get_row_annotation_index = function() {
	INDEX_ENV$I_ROW_ANNOTATION
}

increase_row_annotation_index = function() {
	INDEX_ENV$I_ROW_ANNOTATION = INDEX_ENV$I_ROW_ANNOTATION + 1
}

get_color_mapping_index = function() {
    INDEX_ENV$I_COLOR_MAPPING
}

increase_color_mapping_index = function() {
    INDEX_ENV$I_COLOR_MAPPING = INDEX_ENV$I_COLOR_MAPPING + 1
}

# default colors for matrix or annotations
# this function should be improved later
default_col = function(x, main_matrix = FALSE) {

    if(is.factor(x)) {
        x = as.vector(x)
    }

    if(length(unique(x)) == 1) {
        x = as.character(x)
    }

    attributes(x) = NULL

    x = x[!is.na(x)]

    if(is.character(x)) {  # discrete
        levels = unique(x)
        #colors = hsv(runif(length(levels)), 1-runif(1)/2, 1-runif(1)/2)
        colors = rand_color(length(levels), luminosity = sample(c("bright", "light", "dark", "random"), 1))
        names(colors) = levels
        return(colors)
    } else if(is.numeric(x)) {
        if(main_matrix) {
            if(length(unique(x)) > 100) {
                col_fun = colorRamp2(seq(quantile(x, 0.01), quantile(x, 0.99), length = 3), c("blue", "#EEEEEE", "red"))
            } else {
                col_fun = colorRamp2(seq(min(x), max(x), length = 3), c("blue", "#EEEEEE", "red"))
            }
        } else {
            #col_fun = colorRamp2(range(min(x), max(x)), c("white", hsv(runif(1), 1, 1)))
            col_fun = colorRamp2(range(min(x), max(x)), c("white", rand_color(1, luminosity = sample(c("bright", "dark"), 1))))
        }
        return(col_fun)
    }
}

# == title
# Calculate pairwise distance from a matrix
#
# == param
# -mat a matrix. The distance is calculated by rows.
# -pairwise_fun a function which calculates distance between two vectors.
# -... pass to `stats::as.dist`.
#
# == detail
# You can construct any type of distance measurements by defining a pair-wise distance function.
# The function is implemented by two nested ``for`` loops, so the efficiency may not be so good.
#
# == value
# A `stats::dist` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
dist2 = function(mat, pairwise_fun = function(x, y) sqrt(sum((x - y)^2)), ...) {

    if(!is.matrix(mat)) {
        stop("`mat` should be a matrix.")
    }

    if(nrow(mat) < 2) {
        stop("`mat` should have at least two rows.")
    }

    nr = nrow(mat)
    mat2 = matrix(NA, nrow = nr, ncol = nr)
    rownames(mat2) = colnames(mat2) = rownames(mat)

    for(i in 2:nr) {
        for(j in 1:(nr-1)) {
            mat2[i, j] = pairwise_fun(mat[i, ], mat[j, ])
        }
    }

    as.dist(mat2, ...)
}


get_dist = function(matrix, method) {
    if(is.function(method)) {
        nargs = length(as.list(args(method)))
        if(nargs == 2) { # a distance function
            dst = method(matrix)
        } else if(nargs == 3) {
            dst = dist2(matrix, method)
        } else {
            stop("Since your distance method is a funciton, it can only accept one or two arguments.")
        }
    } else if(method %in% c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")) {
        # if(any(is.na(matrix))) {
        #     dst = get_dist(matrix, function(x, y) {
        #         l = is.na(x) | is.na(y)
        #         x = x[!l]
        #         y = y[!l]
        #         as.vector(dist(rbind(x, y), method = method))
        #     })
        #     warning("NA exists in the matrix, calculating distance by removing NA values.")
        # } else {
            dst = dist(matrix, method = method)
        # }
    } else if(method %in% c("pearson", "spearman", "kendall")) {
        if(any(is.na(matrix))) {
            dst = get_dist(matrix, function(x, y) {
                    l = is.na(x) | is.na(y)
                    x = x[!l]
                    y = y[!l]
                    1 - cor(x, y, method = method)
                })
            warning("NA exists in the matrix, calculating distance by removing NA values.")
        } else {
            dst = switch(method,
                         pearson = as.dist(1 - cor(t(matrix), method = "pearson")),
                         spearman = as.dist(1 - cor(t(matrix), method = "spearman")),
                         kendall = as.dist(1 - cor(t(matrix), method = "kendall")))
        }
    }
    return(dst)
}

get_dend_order = function(x) {
    switch(class(x),
        hclust = x$order,
        dendrogram = order.dendrogram(x))
}

recycle_gp = function(gp, n = 1) {
    for(i in seq_along(gp)) {
        x = gp[[i]]
        gp[[i]] = c(rep(x, floor(n/length(x))), x[seq_len(n %% length(x))])
    }
    return(gp)
}

check_gp = function(gp) {
    if(!inherits(gp, "gpar")) {
        stop("Graphic parameters should be specified by `gpar()`.")
    }
    return(gp)
}


# gp should already be checked by `check_gp`
subset_gp = function(gp, k) {
    gp = lapply(gp, function(x) {
        if(length(x) == 1) x
        else x[k]
    })
    class(gp) = "gpar"
    return(gp)
}


get_text_just = function(rot, side) {
    rot = rot %% 360
    if(! rot %in% c(0, 90, 270)) {
        stop("Only support horizontal or vertical rotations for text.\n")
    }
    if(side == "left") {
        if(rot == 0) {
            return(c(1, 0.5))
        } else if(rot == 90) {
            return(c(0.5, 0))
        } else if(rot == 270) {
            return(c(0.5, 1))
        }
    } else if(side == "right") {
        if(rot == 0) {
            return(c(0, 0.5))
        } else if(rot == 90) {
            return(c(0.5, 1))
        } else if(rot == 270) {
            return(c(0.5, 0))
        }
    } else if(side == "top") {
        if(rot == 0) {
            return(c(0.5, 0))
        } else if(rot == 90) {
            return(c(0, 0.5))
        } else if(rot == 270) {
            return(c(1, 0.5))
        }
    } else if(side == "bottom") {
        if(rot == 0) {
            return(c(0.5, 1))
        } else if(rot == 90) {
            return(c(1, 0.5))
        } else if(rot == 270) {
            return(c(0, 0.5))
        }
    }
}

c.list = function(lt, ..., list = NULL) {
    if(length(lt) == 0) lt = list()

    if(is.null(list)) {
        lt_add = list(...)

        n = length(lt)
        for(i in seq_along(lt_add)) {
            lt[[n+i]] = lt_add[[i]]
        }
    } else {
        lt = c(lt, list)
    }
    return(lt)
}

rep.list = function(x, n) {
    lt = vector("list", n)
    for(i in seq_len(n)) {
        lt[i] = list(x)
    }
    return(lt)
}


list_component = function() {
    vp_name = grid.ls(viewports = TRUE, grobs = FALSE, print = FALSE)$name
}

# == title
# Maximum width of text
#
# == param
# -text a vector of text
# -... pass to `grid::textGrob`
#
# == details
# Simply calculate maximum width of a list of `grid::textGrob` objects.
#
# == value
# A `grid::unit` object.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == seealso
# `max_text_width` is always used to calculate the size of viewport when there is text annotation (`anno_text`)
#
# == example
# x = c("a", "bb", "ccc")
# max_text_width(x, gp = gpar(fontsize = 10))
#
max_text_width = function(text, gp = gpar()) {
    if(is.null(text)) {
        return(unit(0, "mm"))
    }
    n = length(text)
    gp = recycle_gp(gp, n)

    u = max(do.call("unit.c", lapply(seq_len(n), function(i) grobWidth(textGrob(text[i], gp = subset_gp(gp, i))))))
    convertWidth(u, "mm")
}

# == title
# Maximum height of text
#
# == param
# -text a vector of text
# -... pass to `grid::textGrob`
#
# == details
# Simply calculate maximum height of a list of `grid::textGrob` objects.
#
# == value
# A `grid::unit` object.
#
# == seealso
# `max_text_height` is always used to calculate the size of viewport when there is text annotation (`anno_text`)
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# x = c("a", "b\nb", "c\nc\nc")
# max_text_height(x, gp = gpar(fontsize = 10))
#
max_text_height = function(text, gp = gpar()) {
    if(is.null(text)) {
        return(unit(0, "mm"))
    }
    n = length(text)
    gp = recycle_gp(gp, n)

    u = max(do.call("unit.c", lapply(seq_len(n), function(i) grobHeight(textGrob(text[i], gp = subset_gp(gp, i))))))
    convertHeight(u, "mm")
}

dev.null = function(...) {
    pdf(file = NULL, ...)
}

stop_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    stop(x)
}

warning_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    warning(x)
}

message_wrap = function (...) {
    x = paste0(...)
    x = paste(strwrap(x), collapse = "\n")
    message(x)
}

generate_param_list_fun = function(default) {
    if(!is.list(default)) {
        stop("`default` needs to be a list.")
    }
    lt = default
    function(..., list = NULL) {
        if(missing(list)) {
            lt2 = list(...)
        } else {
            lt2 = list
        }
        for(nm in intersect(names(lt), names(lt2))) {
            lt[[nm]] = lt2[[nm]]
        }
        return(lt)
    }
}

add_vp_name = function(vpname) {
    grid.text(vpname, 0, 1, just = c("left", "top"), gp = gpar(fontsize = 6, col = "red"))
}

upViewport = function(...) {
    if(ht_global_opt$show_vp_border) {
        grid.rect(gp = gpar(fill = "transparent", col = "black", lty = 3))
        vpname = current.viewport()$name
        add_vp_name(vpname)
    }
    grid::upViewport(...)
}

popViewport = function(...) {
    if(ht_global_opt$show_vp_border) {
        grid.rect(gp = gpar(fill = "transparent", col = "black", lty = 3))
        vpname = current.viewport()$name
        add_vp_name(vpname)
    }
    grid::popViewport(...)
}


dev.off2 = function () {
    i1 = dev.prev()
    i2 = dev.cur()
    if (i1 > 1)
        dev.set(i1)
    dev.off(i2)
}

unit.c = function(...) {
    lt = list(...)
    lt = lt[!sapply(lt, is.null)]
    do.call(grid::unit.c, lt)
}

cluster_within_group = function(mat, factor, only_order = FALSE) {

    if (!is.factor(factor)) {
        factor = factor(factor, levels = unique(factor))
    }

    dend_list = list()
    order_list = list()
    for(le in unique(levels(factor))) {
        m = mat[, factor == le, drop = FALSE]
        if (ncol(m) == 1) {
            order_list[[le]] = which(factor == le)
            dend_list[[le]] = structure(which(factor == le), class = "dendrogram", leaf = TRUE,
                height = 0, label = 1, members = 1)
        } else {
            hc1 = hclust(dist(t(m)))
            dend_list[[le]] = as.dendrogram(hc1)
            order_list[[le]] = which(factor == le)[order.dendrogram(dend_list[[le]])]
            order.dendrogram(dend_list[[le]]) = order_list[[le]]
        }
    }
    if(only_order) {
        return(unlist(order_list))
    } else {
        parent = as.dendrogram(hclust(dist(t(sapply(order_list, function(x) rowMeans(mat[, x, drop = FALSE]))))))
        dend_list = lapply(dend_list, function(dend) dendrapply(dend, function(node) {
            attr(node, "height") = 0
            node
        }))
        dend = merge(parent, dend_list, reorder = TRUE)
        return(dend)
    }
}


normalize_graphic_param_to_mat = function(x, nc, nr, name) {
    if(is.matrix(x)) {
        if(nrow(x) == nr && ncol(x) == nc) {
            return(x)
        } else {
            stop(paste0(name, "needs to be a matrix with ", nc, " columns and ", nr, " rows."))
        }
    } else {
        if(length(x) == nc) {
            return(matrix(rep(x, each = nr), nc = nc))
        } else if(length(x) == nr) {
            return(matrix(rep(x, times = nc), nc = nc))
        } else if(length(x) == 1) {
            return(matrix(x, nc = nc, nr = nr))
        } else {
            stop(paste0("Since ", name, " is a vector, it should have length of ", nc, " or ", nr, "."))
        }
    }
}

