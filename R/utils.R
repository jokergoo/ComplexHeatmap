
I_HEATMAP = 0
I_ANNOTATION = 0
I_COLUMN_ANNOTATION = 0

get_heatmap_index = function() {
	I_HEATMAP
}

increase_heatmap_index = function() {
	I_HEATMAP <<- I_HEATMAP + 1
}

get_annotation_index = function() {
	I_ANNOTATION
}

increase_annotation_index = function() {
	I_ANNOTATION <<- I_ANNOTATION + 1
}

get_column_annotation_index = function() {
	I_COLUMN_ANNOTATION
}

increase_column_annotation_index = function() {
	I_COLUMN_ANNOTATION <<- I_COLUMN_ANNOTATION + 1
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
        colors = rand_color(length(levels))
        names(colors) = levels
        return(colors)
    } else if(is.numeric(x)) {
        if(main_matrix) {
            col_fun = colorRamp2(seq(min(x), max(x), length.out = 11), 
                rev(brewer.pal(11, "RdYlBu")))
        } else {
            col_fun = colorRamp2(range(min(x), max(x)), c("white", rand_color(1)))
        }
        return(col_fun)
    }
}

grid.dendrogram = function(dend, maxy = attr(dend, "height"), xorder = c("normal", "reverse"), ...) {
    
    is.leaf = function(object) {
        leaf = attr(object, "leaf")
        if(is.null(leaf)) {
            FALSE
        } else {
            leaf
        }
    }
    
    draw.d = function(dend, maxy, xorder = "normal", maxx = 0) {
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
        
        # plot the connection line
        if(xorder == "normal") {
            grid.lines(c(x1, x1), c(y1, height), default.units = "native")
            grid.lines(c(x1, x2), c(height, height), default.units = "native")
            grid.lines(c(x2, x2), c(y2, height), default.units = "native")
        } else {
            grid.lines(maxx - c(x1, x1), c(y1, height), default.units = "native")
            grid.lines(maxx - c(x1, x2), c(height, height), default.units = "native")
            grid.lines(maxx - c(x2, x2), c(y2, height), default.units = "native")
        }
        # do it recursively
        if(!is.leaf(d1)) {
            draw.d(d1, maxy, xorder, maxx)
        }
        if(!is.leaf(d2)) {
            draw.d(d2, maxy, xorder, maxx)
        }
    }
    
    labels = as.character(labels(dend))
    x = seq_along(labels) - 0.5 # leaves are places at x = 0.5, 1.5, ..., n - 0.5

    names(x) = labels
    n = length(labels)

    xorder = match.arg(xorder)[1]
    
    pushViewport(viewport(xscale = c(0, n), yscale = c(0, maxy), ...))
    draw.d(dend, maxy, xorder, maxx= n)
    upViewport()
}
