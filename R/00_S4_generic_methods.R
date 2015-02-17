
if(!isGeneric('add_heatmap')) {
    if(is.function('add_heatmap')) {
        fun = add_heatmap
    } else {
        fun = function(object, ...) standardGeneric('add_heatmap', ...)
    }
    setGeneric('add_heatmap', fun)
}


if(!isGeneric('set_component_height')) {
    if(is.function('set_component_height')) {
        fun = set_component_height
    } else {
        fun = function(object, ...) standardGeneric('set_component_height', ...)
    }
    setGeneric('set_component_height', fun)
}


if(!isGeneric('initialize')) {
    if(is.function('initialize')) {
        fun = initialize
    } else {
        fun = function(object, ...) standardGeneric('initialize', ...)
    }
    setGeneric('initialize', fun)
}


if(!isGeneric('draw_heatmap_body')) {
    if(is.function('draw_heatmap_body')) {
        fun = draw_heatmap_body
    } else {
        fun = function(object, ...) standardGeneric('draw_heatmap_body', ...)
    }
    setGeneric('draw_heatmap_body', fun)
}


if(!isGeneric('draw_hclust')) {
    if(is.function('draw_hclust')) {
        fun = draw_hclust
    } else {
        fun = function(object, ...) standardGeneric('draw_hclust', ...)
    }
    setGeneric('draw_hclust', fun)
}


if(!isGeneric('draw_annotation_legend')) {
    if(is.function('draw_annotation_legend')) {
        fun = draw_annotation_legend
    } else {
        fun = function(object, ...) standardGeneric('draw_annotation_legend', ...)
    }
    setGeneric('draw_annotation_legend', fun)
}


if(!isGeneric('component_height')) {
    if(is.function('component_height')) {
        fun = component_height
    } else {
        fun = function(object, ...) standardGeneric('component_height', ...)
    }
    setGeneric('component_height', fun)
}


if(!isGeneric('map')) {
    if(is.function('map')) {
        fun = map
    } else {
        fun = function(object, ...) standardGeneric('map', ...)
    }
    setGeneric('map', fun)
}


if(!isGeneric('make_layout')) {
    if(is.function('make_layout')) {
        fun = make_layout
    } else {
        fun = function(object, ...) standardGeneric('make_layout', ...)
    }
    setGeneric('make_layout', fun)
}


if(!isGeneric('draw_heatmap_legend')) {
    if(is.function('draw_heatmap_legend')) {
        fun = draw_heatmap_legend
    } else {
        fun = function(object, ...) standardGeneric('draw_heatmap_legend', ...)
    }
    setGeneric('draw_heatmap_legend', fun)
}


if(!isGeneric('heatmap_legend_size')) {
    if(is.function('heatmap_legend_size')) {
        fun = heatmap_legend_size
    } else {
        fun = function(object, ...) standardGeneric('heatmap_legend_size', ...)
    }
    setGeneric('heatmap_legend_size', fun)
}


if(!isGeneric('annotation_legend_size')) {
    if(is.function('annotation_legend_size')) {
        fun = annotation_legend_size
    } else {
        fun = function(object, ...) standardGeneric('annotation_legend_size', ...)
    }
    setGeneric('annotation_legend_size', fun)
}


if(!isGeneric('draw_annotation')) {
    if(is.function('draw_annotation')) {
        fun = draw_annotation
    } else {
        fun = function(object, ...) standardGeneric('draw_annotation', ...)
    }
    setGeneric('draw_annotation', fun)
}


if(!isGeneric('draw_dimnames')) {
    if(is.function('draw_dimnames')) {
        fun = draw_dimnames
    } else {
        fun = function(object, ...) standardGeneric('draw_dimnames', ...)
    }
    setGeneric('draw_dimnames', fun)
}


if(!isGeneric('color_mapping_legend')) {
    if(is.function('color_mapping_legend')) {
        fun = color_mapping_legend
    } else {
        fun = function(object, ...) standardGeneric('color_mapping_legend', ...)
    }
    setGeneric('color_mapping_legend', fun)
}


if(!isGeneric('draw')) {
    if(is.function('draw')) {
        fun = draw
    } else {
        fun = function(object, ...) standardGeneric('draw', ...)
    }
    setGeneric('draw', fun)
}


if(!isGeneric('draw_title')) {
    if(is.function('draw_title')) {
        fun = draw_title
    } else {
        fun = function(object, ...) standardGeneric('draw_title', ...)
    }
    setGeneric('draw_title', fun)
}


if(!isGeneric('component_width')) {
    if(is.function('component_width')) {
        fun = component_width
    } else {
        fun = function(object, ...) standardGeneric('component_width', ...)
    }
    setGeneric('component_width', fun)
}


if(!isGeneric('draw_heatmap_list')) {
    if(is.function('draw_heatmap_list')) {
        fun = draw_heatmap_list
    } else {
        fun = function(object, ...) standardGeneric('draw_heatmap_list', ...)
    }
    setGeneric('draw_heatmap_list', fun)
}

