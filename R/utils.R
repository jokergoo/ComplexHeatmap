
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