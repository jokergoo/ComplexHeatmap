
INDEX_ENV = new.env()

INDEX_ENV$I_HEATMAP = 0
INDEX_ENV$I_ANNOTATION = 0
INDEX_ENV$I_ROW_ANNOTATION = 0

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

    if(is.character(x)) {  # discrete
        levels = unique(x)
        colors = rainbow_hcl(length(levels), c = 60, l = 75)
        names(colors) = levels
        return(colors)
    } else if(is.numeric(x)) {
        if(main_matrix) {
            if(length(unique(x) > 100)) {
                col_fun = colorRamp2(seq(quantile(x, 0.05), quantile(x, 0.95), length.out = 100), diverge_hcl(100, c = 100, l = c(50, 90), power = 1))
            } else {
                col_fun = colorRamp2(seq(min(x), max(x), length.out = 100), diverge_hcl(100, c = 100, l = c(50, 90), power = 1))
            }
        } else {
            col_fun = colorRamp2(range(min(x), max(x)), c("white", rainbow_hcl(1, c = 60, l = 75)))
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
# -max_height maximum height of the dendrogram. It is useful if you want to plot more than one dendrograms.
# -order should leaves of dendrogram be put in the normal order or reverse order?
# -... pass to `grid::viewport` that contains the dendrogram.
#
# == details
# The dendrogram tree can be renderred (e.g. by ``dendextend`` package).
#
# A viewport is created which contains the dendrogram.
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

    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }
    
    draw.d = function(dend, max_height, facing = "bottom", order = "normal", max_width = 0) {
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
        edge_gp1 = do.call("gpar", as.list(attr(d1, "edgePar")))
        edge_gp2 = do.call("gpar", as.list(attr(d2, "edgePar")))

        # plot the connection line
        if(order == "normal") {
            if(facing == "bottom") {
                grid.lines(c(x1, x1, (x1+x2)/2), c(y1, height, height), default.units = "native", gp = edge_gp1)
                grid.lines(c(x2, x2, (x1+x2)/2), c(y2, height, height), default.units = "native", gp = edge_gp2)
            } else if(facing == "top") {
                grid.lines(c(x1, x1, (x1+x2)/2), max_height - c(y1, height, height), default.units = "native", gp = edge_gp1)
                grid.lines(c(x2, x2, (x1+x2)/2), max_height - c(y2, height, height), default.units = "native", gp = edge_gp2)
            } else if(facing == "right") {
                grid.lines(max_height - c(y1, height, height), c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                grid.lines(max_height - c(y2, height, height), c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
            } else if(facing == "left") {
                grid.lines(c(y1, height, height), c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                grid.lines(c(y2, height, height), c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
            }
        } else {
            if(facing == "bottom") {
                grid.lines(max_width - c(x1, x1, (x1+x2)/2), c(y1, height, height), default.units = "native", gp = edge_gp1)
                grid.lines(max_width - c(x2, x2, (x1+x2)/2), c(y2, height, height), default.units = "native", gp = edge_gp2)
            } else if(facing == "top") {
                grid.lines(max_width - c(x1, x1, (x1+x2)/2), max_height - c(y1, height, height), default.units = "native", gp = edge_gp1)
                grid.lines(max_width - c(x2, x2, (x1+x2)/2), max_height - c(y2, height, height), default.units = "native", gp = edge_gp2)
            } else if(facing == "right") {
                grid.lines(max_height - c(y1, height, height), max_width - c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                grid.lines(max_height - c(y2, height, height), max_width - c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
            } else if(facing == "left") {
                grid.lines(c(y1, height, height), max_width - c(x1, x1, (x1+x2)/2), default.units = "native", gp = edge_gp1)
                grid.lines(c(y2, height, height), max_width - c(x2, x2, (x1+x2)/2), default.units = "native", gp = edge_gp2)
            }
        }
        # do it recursively
        if(!is.leaf(d1)) {
            draw.d(d1, max_height, facing, order, max_width)
        }
        if(!is.leaf(d2)) {
            draw.d(d2, max_height, facing, order, max_width)
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
# Calculate distance from a matrix
#
# == param
# -mat a matrix. The distance is calculated by rows.
# -pairwise_fun a function which calculates distance between two vectors.
# -... pass to `stats::dist`.
#
# == detail
# You can construct any type of distance measurements by defining a pair-wise distance function.
# The function is implemented by two nested ``for`` loops, thus the efficiency may not be so good.
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
        dst = dist(matrix, method = method)
    } else if(method %in% c("pearson", "spearman", "kendall")) {
        dst = switch(method,
                     pearson = as.dist(1 - cor(t(matrix), method = "pearson")),
                     spearman = as.dist(1 - cor(t(matrix), method = "spearman")),
                     kendall = as.dist(1 - cor(t(matrix), method = "kendall")))
    }
    return(dst)
}

get_hclust_order = function(x) {
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
    gp = lapply(gp, function(x) x[k])
    class(gp) = "gpar"
    return(gp)
}
