
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
# Draw dendrogram under grid system
#
# == param
# -dend a `stats::dendrogram` object.
# -facing facing of the dendrogram.
# -max_height maximum height of the dendrogram. It is useful to make dendrograms comparable
#             if you want to plot more than one dendrograms. Height for each dendrogram can be obtained by
#             ``attr(dend, "height")``.
# -order should leaves of dendrogram be put in the normal order (1, ..., n) or reverse order (n, ..., 1)?
#        It may matters for the dendrograms putting on left and right.
# -... pass to `grid::viewport` which contains the dendrogram.
#
# == details
# The dendrogram can be renderred (e.g. by ``dendextend`` package).
#
# A viewport is created which contains the dendrogram.
#
# This function only plots the dendrogram without adding labels. The leaves of the dendrogram
# locates at ``unit(c(0.5, 1.5, ...(n-0.5))/n, "npc")``.
#
# == value
# No value is returned.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
grid.dendrogram = function(dend, facing = c("bottom", "top", "left", "right"), 
    max_height = NULL, order = c("normal", "reverse"), ...) {
    
    facing = match.arg(facing)[1]

    if(is.null(max_height)) {
        max_height = attr(dend, "height")
    }
    
    if(max_height == 0) {
    	return(invisible(NULL))
    }

    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }

    n = nobs(dend)

    .env = new.env()
    .env$k = 0
    modify_labels = function(de) {
        for(i in seq_len(length(de))) {
            d = de[[i]]
            if(is.leaf(d)) {
                .env$k = .env$k + 1
                attr(d, "label") = paste0(attr(d, "label"), .env$k)
                de[[i]] = d
            } else {
                de[[i]] = modify_labels(d)
            }
        }
        return(de)
    }

    dend = modify_labels(dend)
    
    draw.d = function(dend, max_height, facing = "bottom", order = "normal", max_width = 0, env = NULL) {
        leaf = attr(dend, "leaf")
        d1 = dend[[1]]  # child tree 1
        d2 = dend[[2]]  # child tree 2
        height = attr(dend, "height")
        midpoint = attr(dend, "midpoint")
        
        if(is.leaf(d1)) {
            x1 = x[as.character(attr(d1, "label"))]
        } else {
            x1 = attr(d1, "midpoint") + x[as.character(labels(d1))[1]]
        }
        y1 = attr(d1, "height")
        
        if(is.leaf(d2)) {
            x2 = x[as.character(attr(d2, "label"))]
        } else {
            x2 = attr(d2, "midpoint") + x[as.character(labels(d2))[1]]
        }
        y2 = attr(d2, "height")

        # graphic parameters for current branch
        edge_gp1 = as.list(attr(d1, "edgePar"))
        edge_gp2 = as.list(attr(d2, "edgePar"))

        if(is.null(env)) {
            begin = TRUE
            env = new.env()
            n = nobs(dend)
            env$x0 = NULL
            env$y0 = NULL
            env$x1 = NULL
            env$y1 = NULL
            env$col = NULL
            env$lty = NULL
            env$lwd = NULL
        } else {
            begin = FALSE
        }

        for(gp_name in c("col", "lwd", "lty")) {
            if(is.null(edge_gp1[[gp_name]])) {
                env[[gp_name]] = c(env[[gp_name]], rep(get.gpar(gp_name)[[gp_name]], 2))
            } else {
                env[[gp_name]] = c(env[[gp_name]], rep(edge_gp1[[gp_name]], 2))
            }
            if(is.null(edge_gp2[[gp_name]])) {
                env[[gp_name]] = c(env[[gp_name]], rep(get.gpar(gp_name)[[gp_name]], 2))
            } else {
                env[[gp_name]] = c(env[[gp_name]], rep(edge_gp2[[gp_name]], 2))
            }
        }


        # plot the connection line
        if(order == "normal") {
            if(facing == "bottom") {
                # grid.lines(c(x1, x1, (x1+x2)/2), c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(c(x2, x2, (x1+x2)/2), c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(x1, x1, x2, x2))
                env$y0 = c(env$y0, c(y1, height, y2, height))
                env$x1 = c(env$x1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, c(height, height, height, height))
            } else if(facing == "top") {
                # grid.lines(c(x1, x1, (x1+x2)/2), max_height - c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(c(x2, x2, (x1+x2)/2), max_height - c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(x1, x1, x2, x2))
                env$y0 = c(env$y0, max_height - c(y1, height, y2, height))
                env$x1 = c(env$x1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, max_height - c(height, height, height, height))
            } else if(facing == "right") {
                # grid.lines(max_height - c(y1, height, height), c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(max_height - c(y2, height, height), c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_height - c(y1, height, y2, height))
                env$y0 = c(env$y0, c(x1, x1, x2, x2))
                env$x1 = c(env$x1, max_height - c(height, height, height, height))
                env$y1 = c(env$y1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            } else if(facing == "left") {
                # grid.lines(c(y1, height, height), c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(c(y2, height, height), c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(y1, height, y2, height))
                env$y0 = c(env$y0, c(x1, x1, x2, x2))
                env$x1 = c(env$x1, c(height, height, height, height))
                env$y1 = c(env$y1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            }
        } else {
            if(facing == "bottom") {
                # grid.lines(max_width - c(x1, x1, (x1+x2)/2), c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(max_width - c(x2, x2, (x1+x2)/2), c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_width - c(x1, x1, x2, x2))
                env$y0 = c(env$y0, c(y1, height, y2, height))
                env$x1 = c(env$x1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, c(height, height, height, height))
            } else if(facing == "top") {
                # grid.lines(max_width - c(x1, x1, (x1+x2)/2), max_height - c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(max_width - c(x2, x2, (x1+x2)/2), max_height - c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_width - c(x1, x1, x2, x2))
                env$y0 = c(env$y0, max_height - c(y1, height, y2, height))
                env$x1 = c(env$x1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, max_height - c(height, height, height, height))
            } else if(facing == "right") {
                # grid.lines(max_height - c(y1, height, height), max_width - c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(max_height - c(y2, height, height), max_width - c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_height - c(y1, height, y2, height))
                env$y0 = c(env$y0, max_width - c(x1, x1, x2, x2))
                env$x1 = c(env$x1, max_height - c(height, height, height, height))
                env$y1 = c(env$y1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            } else if(facing == "left") {
                # grid.lines(c(y1, height, height), max_width - c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(c(y2, height, height), max_width - c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(y1, height, y2, height))
                env$y0 = c(env$y0, max_width - c(x1, x1, x2, x2))
                env$x1 = c(env$x1, c(height, height, height, height))
                env$y1 = c(env$y1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            }
        }
        # do it recursively
        if(!is.leaf(d1)) {
            draw.d(d1, max_height, facing, order, max_width, env = env)
        }
        if(!is.leaf(d2)) {
            draw.d(d2, max_height, facing, order, max_width, env = env)
        }

        if(begin) {
            grid.segments(env$x0, env$y0, env$x1, env$y1, default.units = "native", gp = gpar(col = env$col, lty = env$lty, lwd = env$lwd))
        }
    }

    labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5 # leaves are places at x = 0.5, 1.5, ..., n - 0.5

    names(x) = labels
    n = length(labels)

    order = match.arg(order)[1]
    
    if(facing %in% c("top", "bottom")) {
        pushViewport(viewport(xscale = c(0, n), yscale = c(0, max_height), ...))
        draw.d(dend, max_height, facing, order, max_width = n)
        upViewport()
    } else if(facing %in% c("right", "left")) {
        pushViewport(viewport(yscale = c(0, n), xscale = c(0, max_height), ...))
        draw.d(dend, max_height, facing, order, max_width = n)
        upViewport()
    }
}

# == title
# Adjust dendrogram based on width of leaves
#
# == param
# -dend a `stats::dendrogram` object.
# -width a vector of width. The order of width SHOULD be same as the order of original elements before clustering.
# -offset offset to x = 0
#
# == details
# In the standard `stats::dendrogram` object, leaves locate at x = 0.5, 1.5, ..., n - 0.5,
# which means, the width of leaves are always 1 and the distance to neighbouring leaves is always 1 as well.
# Here `adjust_dend_by_leaf_width` adjusts the dendrogram by setting different width for leaves so that leaves
# have unequal distance to other leaves.
#
# The adjusted dendrogram can be sent to `grid.dendrogram2` to make the dendrogram.
#
# For each branch as well each leaf, a new attribute of ``x`` is added which is the position of the middle point or the leaf.
# For each leaf, a new attribute of ``width`` is added which is the width of current leaf.
#
# == value
# A `stats::dendrogram` object. The adjustment will not affect other standard dendrogram functions.
#
# == author
# Zuguang Gu <z.gu@dkfz.de>
#
# == example
# m = matrix(rnorm(100), 10)
# dend = as.dendrogram(hclust(dist(m)))
# dend = adjust_dend_by_leaf_width(dend, width = 1:10)
# require(dendextend)
# get_leaves_attr(dend, "label")
# get_leaves_attr(dend, "width")
# get_leaves_attr(dend, "x")
adjust_dend_by_leaf_width = function(dend, width = 1, offset = 0) {
    n = nobs(dend)

    if(length(width) == 1) {
        width = rep(width, n)
    }

    if(identical(width, rep(1, n))) {
        return(dend)
    }

    if(length(width) != n) {
        stop("length of `width` should be same as `dend`.")
    }

    dend_order = order.dendrogram(dend)
    leaves_pos = cumsum(width[dend_order]) - width[dend_order]/2 + offset
    label2index = structure(dend_order, names = labels(dend))

    env = new.env()
    env$dend = dend

    adj_dend = function(ind = NULL) {
        if(is.null(ind)) {
            d = env$dend
        } else {
            d = env$dend[[ind]]
        }
        
        if(is.leaf(d)) {
            i = which(labels(dend) == attr(d, "label"))
            x = leaves_pos[i]
            attr(env$dend[[ind]], "width") = width[dend_order[i]]
        } else {
            x = ( adj_dend(c(ind, 1)) + adj_dend(c(ind, 2)) )/2
        }

        if(is.null(ind)) {
            attr(env$dend, "x") = x
        } else {
            attr(env$dend[[ind]], "x") = x
        }
        return(x)
    }

    adj_dend()
    attr(env$dend, "width_adjusted") = TRUE
    attr(env$dend, "offset") = offset

    return(env$dend)
}


# == title
# Draw dendrogram under grid system
#
# == param
# -dend a `stats::dendrogram` object which has been adjusted by `adjust_dend_by_leaf_width`, or else
#      it will be sent back to `grid.dendrogram`.
# -facing same as in `grid.dendrogram`.
# -max_height same as in `grid.dendrogram`.
# -order same as in `grid.dendrogram`.
# -... same as in `grid.dendrogram`.
#
# == author
# Zuguang gu <z.gu@dkfz.de>
#
# == example
# m = matrix(rnorm(100), 10)
# dend = as.dendrogram(hclust(dist(m)))
# dend = adjust_dend_by_leaf_width(dend, width = 1:10)
# grid.dendrogram2(dend)
grid.dendrogram2 = function(dend, facing = c("bottom", "top", "left", "right"), 
    max_height = NULL, order = c("normal", "reverse"), ...) {

    if(is.null(attr(dend, "width_adjusted"))) {
        grid.dendrogram(dend, facing = facing, max_height = max_height, order = order, ...)
        return(invisible(NULL))
    }
    
    facing = match.arg(facing)[1]

    if(is.null(max_height)) {
        max_height = attr(dend, "height")
    }
    
    if(max_height == 0) {
        return(invisible(NULL))
    }

    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }
    
    draw.d = function(dend, max_height, facing = "bottom", order = "normal", max_width = 0, env = NULL) {

        d1 = dend[[1]]  # child tree 1
        d2 = dend[[2]]  # child tree 2
        height = attr(dend, "height")
        
        x1 = attr(d1, "x")
        y1 = attr(d1, "height")
        
        x2 = attr(d2, "x")
        y2 = attr(d2, "height")
        midpoint = (x1 + x2)/2

        # graphic parameters for current branch
        edge_gp1 = as.list(attr(d1, "edgePar"))
        edge_gp2 = as.list(attr(d2, "edgePar"))

        if(is.null(env)) {
            begin = TRUE
            env = new.env()
            n = nobs(dend)
            env$x0 = NULL
            env$y0 = NULL
            env$x1 = NULL
            env$y1 = NULL
            env$col = NULL
            env$lty = NULL
            env$lwd = NULL
        } else {
            begin = FALSE
        }

        for(gp_name in c("col", "lwd", "lty")) {
            if(is.null(edge_gp1[[gp_name]])) {
                env[[gp_name]] = c(env[[gp_name]], rep(get.gpar(gp_name)[[gp_name]], 2))
            } else {
                env[[gp_name]] = c(env[[gp_name]], rep(edge_gp1[[gp_name]], 2))
            }
            if(is.null(edge_gp2[[gp_name]])) {
                env[[gp_name]] = c(env[[gp_name]], rep(get.gpar(gp_name)[[gp_name]], 2))
            } else {
                env[[gp_name]] = c(env[[gp_name]], rep(edge_gp2[[gp_name]], 2))
            }
        }

        # plot the connection line
        if(order == "normal") {
            if(facing == "bottom") {
                # grid.lines(c(x1, x1, (x1+x2)/2), c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(c(x2, x2, (x1+x2)/2), c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(x1, x1, x2, x2))
                env$y0 = c(env$y0, c(y1, height, y2, height))
                env$x1 = c(env$x1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, c(height, height, height, height))
            } else if(facing == "top") {
                # grid.lines(c(x1, x1, (x1+x2)/2), max_height - c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(c(x2, x2, (x1+x2)/2), max_height - c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(x1, x1, x2, x2))
                env$y0 = c(env$y0, max_height - c(y1, height, y2, height))
                env$x1 = c(env$x1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, max_height - c(height, height, height, height))
            } else if(facing == "right") {
                # grid.lines(max_height - c(y1, height, height), c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(max_height - c(y2, height, height), c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_height - c(y1, height, y2, height))
                env$y0 = c(env$y0, c(x1, x1, x2, x2))
                env$x1 = c(env$x1, max_height - c(height, height, height, height))
                env$y1 = c(env$y1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            } else if(facing == "left") {
                # grid.lines(c(y1, height, height), c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(c(y2, height, height), c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(y1, height, y2, height))
                env$y0 = c(env$y0, c(x1, x1, x2, x2))
                env$x1 = c(env$x1, c(height, height, height, height))
                env$y1 = c(env$y1, c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            }
        } else {
            if(facing == "bottom") {
                # grid.lines(max_width - c(x1, x1, (x1+x2)/2), c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(max_width - c(x2, x2, (x1+x2)/2), c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_width - c(x1, x1, x2, x2))
                env$y0 = c(env$y0, c(y1, height, y2, height))
                env$x1 = c(env$x1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, c(height, height, height, height))
            } else if(facing == "top") {
                # grid.lines(max_width - c(x1, x1, (x1+x2)/2), max_height - c(y1, height, height), default.units = "native", gp = edge_gp1)
                # grid.lines(max_width - c(x2, x2, (x1+x2)/2), max_height - c(y2, height, height), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_width - c(x1, x1, x2, x2))
                env$y0 = c(env$y0, max_height - c(y1, height, y2, height))
                env$x1 = c(env$x1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
                env$y1 = c(env$y1, max_height - c(height, height, height, height))
            } else if(facing == "right") {
                # grid.lines(max_height - c(y1, height, height), max_width - c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(max_height - c(y2, height, height), max_width - c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, max_height - c(y1, height, y2, height))
                env$y0 = c(env$y0, max_width - c(x1, x1, x2, x2))
                env$x1 = c(env$x1, max_height - c(height, height, height, height))
                env$y1 = c(env$y1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            } else if(facing == "left") {
                # grid.lines(c(y1, height, height), max_width - c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                # grid.lines(c(y2, height, height), max_width - c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
                env$x0 = c(env$x0, c(y1, height, y2, height))
                env$y0 = c(env$y0, max_width - c(x1, x1, x2, x2))
                env$x1 = c(env$x1, c(height, height, height, height))
                env$y1 = c(env$y1, max_width - c(x1, (x1+x2)/2, x2, (x1+x2)/2))
            }
        }
        # do it recursively
        if(!is.leaf(d1)) {
            draw.d(d1, max_height, facing, order, max_width, env = env)
        } else {
            grid.rect(x1, width = attr(d1, "width"), default.units = "native", gp = gpar(fill = rand_color(1, transparency = 0.5)))
        }
        if(!is.leaf(d2)) {
            draw.d(d2, max_height, facing, order, max_width, env = env)
        } else {
            grid.rect(x2, width = attr(d2, "width"), default.units = "native", gp = gpar(fill = rand_color(1, transparency = 0.5)))
        }

        if(begin) {
            grid.segments(env$x0, env$y0, env$x1, env$y1, default.units = "native", gp = gpar(col = env$col, lty = env$lty, lwd = env$lwd))
        }
    }

    sum_width = sum(.get_leaves_width(dend))
    order = match.arg(order)[1]
    offset = attr(dend, "offset")
    
    if(facing %in% c("top", "bottom")) {
        pushViewport(viewport(xscale = c(0, sum_width) + offset, yscale = c(0, max_height), ...))
        draw.d(dend, max_height, facing, order, max_width = sum_width)
        upViewport()
    } else if(facing %in% c("right", "left")) {
        pushViewport(viewport(yscale = c(0, sum_width) + offset, xscale = c(0, max_height), ...))
        draw.d(dend, max_height, facing, order, max_width = sum_width)
        upViewport()
    }
}

.get_leaves_width = function(d, v = NULL) {
    if(is.leaf(d)) {
        v = c(v, attr(d, "width"))
    } else {
        v = .get_leaves_width(d[[1]], v)
        v = .get_leaves_width(d[[2]], v)
    }
    return(v)
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

# can only cut dendrogram for which branches at every node are two
cut_dendrogram = function(dend, k) {
    h = sort(dend_branches_heights(dend), decreasing = TRUE)
    height = (h[k-1] + h[k])/2
    trees = cut(dend, h = height)
    trees$lower
}

dend_branches_heights = function(d, v = NULL) {
    if(!is.leaf(d)) {
        v = c(v, attr(d, "height"))
        v = dend_branches_heights(d[[1]], v)
        v = dend_branches_heights(d[[2]], v)
    }
    return(v)
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
max_text_width = function(text, ...) {
    if(is.null(text)) {
        return(unit(0, "mm"))
    }
    max(do.call("unit.c", lapply(text, function(x) grobWidth(textGrob(x, ...)))))
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
max_text_height = function(text, ...) {
    if(is.null(text)) {
        return(unit(0, "mm"))
    }
    max(do.call("unit.c", lapply(text, function(x) grobHeight(textGrob(x, ...)))))
}
