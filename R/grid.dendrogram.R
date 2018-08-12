
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

############
## for these functions, plotting dendrogram does not reply on the midpoint attribute in the
## dendrogram object, the positions of all nodes are calculated and stored as x attribute


subset_dendrogram = function(x, ind) {
    if(is.null(ind)) {
        return(x)
    } else {
        x[[ind]]
    }
}

adjust_dend_by_x = function(dend, x = 1:nobs(dend)-0.5) {
    n = nobs(dend)

    if(length(x) != n) {
        stop("`x` should be a vector with same length as `dend`.")
    }

    dend_order = order.dendrogram(dend)
    leaves_pos = x
    od2index = NULL
    od2index[dend_order] = 1:n

    env = new.env()
    env$dend = dend

    adj_dend = function(ind = NULL) {
        d = subset_dendrogram(env$dend, ind)
        n_node = length(d)

        if(is.leaf(d)) {
            i = od2index[ d[][[1]] ]
            x = leaves_pos[i]
            
        } else {
            nc = length(d)
            for(i in seq_len(nc)) {
                adj_dend(c(ind, i))
            }
            d = subset_dendrogram(env$dend, ind)

            xl = lapply(1:nc, function(i) attr(d[[i]], "x"))
            if(all(sapply(xl, inherits, "unit"))) {
                xl = do.call("unit.c", xl)
            } else {
                xl = unlist(xl)
            }
            x = (max(xl) + min(xl))*0.5
        }

        if(is.null(ind)) {
            attr(env$dend, "x") = x
        } else {
            attr(env$dend[[ind]], "x") = x
        }
        return(x)
    }

    adj_dend()

    dend = env$dend
    return(dend)
}

unit.c = function(...) {
    lt = list(...)
    lt = lt[!sapply(lt, is.null)]
    do.call(grid::unit.c, lt)
}

# the direction of the dendrogram is facing bottom, 
construct_dend_segments = function(dend, gp) {

    if(is.null(attr(dend, "x"))) {
        dend = adjust_dend_by_x(dend)
    }

    x_is_unit = inherits(attr(dend, "x"), "unit")
    height_is_unit = inherits(attr(dend, "height"), "unit")

    env = new.env()
    env$x0 = NULL
    env$y0 = NULL
    env$x1 = NULL
    env$y1 = NULL
    env$col = NULL
    env$lty = NULL
    env$lwd = NULL
    generate_children_dendrogram_segments = function(dend, env = NULL) {

        if(is.leaf(dend)) {
            return(NULL)
        }

        height = attr(dend, "height")
        nc = length(dend)
        
        xl = lapply(seq_len(nc), function(i) attr(dend[[i]], "x"))
        yl = lapply(seq_len(nc), function(i) attr(dend[[i]], "height"))
        if(x_is_unit) {
            xl = do.call("unit.c", xl)
        } else {
            xl = unlist(xl)
        }
        if(height_is_unit) {
            yl = do.call("unit.c", yl)
        } else {
            yl = unlist(yl)
        }
            
        max_x = max(xl)
        min_x = min(xl)
        mid_x = (max_x + min_x)*0.5

        # graphic parameters for current branch
        edge_gp_list = lapply(seq_len(nc), function(i) as.list(attr(dend[[i]], "edgePar")))
        for(i in c(setdiff(seq_len(nc), c(1, nc)), c(1, nc))) {
            for(gp_name in c("col", "lwd", "lty")) {
                # gp for two segments
                if(is.null(edge_gp_list[[i]][[gp_name]])) {
                    gpa = rep(get.gpar(gp_name)[[gp_name]], 2)
                } else {
                    gpa = rep(edge_gp_list[[i]][[gp_name]], 2)
                }

                env[[gp_name]] = c(env[[gp_name]], gpa)
            }

            if(x_is_unit) {
                env$x0 = unit.c(env$x0, xl[i], xl[i])
                env$x1 = unit.c(env$x1, xl[i], mid_x)
            } else {
                env$x0 = c(env$x0, xl[i], xl[i])
                env$x1 = c(env$x1, xl[i], mid_x)
            }
            if(height_is_unit) {
                env$y0 = unit.c(env$y0, yl[i], height)
                env$y1 = unit.c(env$y1, height, height)
            } else {
                env$y0 = c(env$y0, yl[i], height)
                env$y1 = c(env$y1, height, height)
            }
        }

        for(i in seq_len(nc)) {
            generate_children_dendrogram_segments(dend[[i]], env)
        }
        
    }

    generate_children_dendrogram_segments(dend, env)

    lt = as.list(env)

    if("col" %in% names(gp)) {
        lt$col = gp$col
    }
    if("lwd" %in% names(gp)) {
        lt$lwd = gp$lwd
    }
    if("lty" %in% names(gp)) {
        lt$lty = gp$lty
    }
    return(lt)

}

dendrogramGrob = function(dend, facing = c("bottom", "top", "left", "right"),
    order = c("normal", "reverse"), gp = gpar()) {

    facing = match.arg(facing)[1]
    order = match.arg(order)[1]

    lt = construct_dend_segments(dend, gp)

    is_x_unit = inherits(lt$x0[1], "unit")

    if(is_x_unit) {
        if(order == "reverse") {
            lt$x0 = unit(1, "npc") - lt$x0
            lt$x1 = unit(1, "npc") - lt$x1
        }
    } else {
        xlim = range(lt[c("x0", "x1")])
        if(order == "reverse") {
            lt$x0 = unit(1, "npc") - unit(lt$x0, "native")
            lt$x1 = unit(1, "npc") - unit(lt$x1, "native")
        }
    }

    if(facing %in% c("top", "right")) {
        lt$y0 = unit(1, "npc") - unit(lt$y0, "native")
        lt$y1 = unit(1, "npc") - unit(lt$y1, "native")
    }
    if(facing %in% c("bottom", "top")) {
        segmentsGrob(lt$x0, lt$y0, lt$x1, lt$y1, gp = gpar(lwd = lt$lwd, lty = lt$lty, col = lt$col),
            default.units = "native")
    } else if(facing %in% c("left", "right")) {
        segmentsGrob(lt$y0, lt$x0, lt$y1, lt$x1, gp = gpar(lwd = lt$lwd, lty = lt$lty, col = lt$col),
            default.units = "native")
    }
}

grid.dendrogram = function(dend, ..., test = FALSE) {
    gb = dendrogramGrob(dend, ...)
    if(test) {
        ylim = range(gb[c("y0", "y1")])
        ylim[1] = - 0.05*(ylim[2] - ylim[1])
        ylim[2] = ylim[2] + 0.05*ylim[2]
        grid.newpage()
        if(is.unit(gb$x0[1])) {
            width = max(unit.c(gb$x0, gb$x1))
            pushViewport(viewport(yscale = ylim, 
                width = width*1.1, 
                height = unit(1, "npc") - unit(4, "cm")))
        } else {
            xlim = range(gb[c("x0", "x1")])
            xlim[1] = xlim[1] - 0.05*(xlim[2] - xlim[1])
            xlim[2] = xlim[2] + 0.05*(xlim[2] - xlim[1])
            pushViewport(viewport(xscale = xlim, yscale = ylim, 
                width = unit(1, "npc") - unit(4, "cm"), 
                height = unit(1, "npc") - unit(4, "cm")))
        }
        
    }
    grid.draw(gb)
    if(test) {
        grid::grid.xaxis()
        grid::grid.yaxis()
        grid.rect()
        popViewport()
    }
}

merge.dendrogram = function(x, y, only_parent = FALSE, ...) {

    parent = x
    children = y

    n = nobs(parent)
    if(n != length(children)) {
        stop("Number of children dendrograms should be same as leaves in parent.")
    }

    # adjust height of parent dendrogram
    children_height = sapply(children, function(x) attr(x, "height"))
    children_height_max = max(children_height)
    parent_height = attr(parent, "height")
    parent_height_min = get_branches_heights(parent)
    h_line = children_height_max + parent_height_min*0.1

    od2index = NULL
    od2index[order.dendrogram(parent)] = 1:n

    env = new.env()
    env$dend = parent
    update_dend_height_in_parent = function(ind = NULL) {
        
        if(is.null(ind)) {
            dend = env$dend
            if(is.leaf(dend)) {
                attr(env$dend, "height") = children_height[ od2index[dend[][[1]]] ]
            } else {
                attr(env$dend, "height") = attr(dend, "height") + children_height_max
            }
        } else {
            dend = env$dend[[ind]]
            if(is.leaf(dend)) {
                attr(env$dend[[ind]], "height") = children_height[ od2index[dend[][[1]]] ]
            } else {
                attr(env$dend[[ind]], "height") = attr(dend, "height") + children_height_max
            }
        }

        if(is.leaf(dend)) {
            return(NULL)
        }

        nc = length(dend)
        for(i in seq_len(nc)) {
            update_dend_height_in_parent(c(ind, i))
        }
    }
    update_dend_height_in_parent(NULL)

    if(only_parent) {
        return(env$dend)
    }

    # merge with children
    merge_with_children = function(ind) {
        if(is.null(ind)) {
            dend = env$dend
        } else {
            dend = env$dend[[ind]]
        }

        if(is.leaf(dend)) {
            i = dend[][[1]]
            dend = children[[i]]
              if(is.null(ind)) {
                env$dend = dend
            } else {
                env$dend[[ind]] = dend
            }
            return(NULL)
        } else {
            nc = length(dend)
            for(i in seq_len(nc)) {
                merge_with_children(c(ind, i))
            }
        }
    }
    merge_with_children(NULL)
    dend = env$dend

    children_members = sapply(children, function(x) attr(x, "members"))
    attr(dend, "members") = sum(children_members)

    # adjust order of leaves
    od_parent = order.dendrogram(parent)
    od_children = lapply(children, order.dendrogram)

    s = 0
    for(i in seq_along(od_parent)) {
        od_children[[ od_parent[i] ]] = od_children[[ od_parent[i] ]] + s
        s = s + length(od_children[[ od_parent[i] ]])
    }

    order.dendrogram(dend) = unlist(od_children)

    attr(dend, "children_height") = children_height
    attr(dend, "parent_height") = parent_height
    attr(dend, "h_line") = h_line

    return(dend)
}

get_branches_heights = function(dend) {
    if(is.leaf(dend)) {
        return(NULL)
    } else {
        c(attr(dend, "height"), unlist(sapply(dend, get_branches_heights)))
    }
}

"order.dendrogram<-" = function(x, value) {
    map = NULL
    n = nobs(x)
    map[ order.dendrogram(x) ] = seq_len(n)
    dendrapply(x, function(node) {
        if(is.leaf(node)) {
            node[[1]] = value[ map[ node[[1]] ] ]
        }
        return(node)
    })
}

print.dendrogram = function(x) {
    str(x)
}


# can only cut dendrogram for which branches at every node are two
cut_dendrogram = function(dend, k) {
    h = sort(dend_branches_heights(dend), decreasing = TRUE)
    height = (h[k-1] + h[k])/2
    trees = cut(dend, h = height)
    trees
}

dend_branches_heights = function(d, v = NULL) {
    if(!is.leaf(d)) {
        v = c(v, attr(d, "height"))
        v = dend_branches_heights(d[[1]], v)
        v = dend_branches_heights(d[[2]], v)
    }
    return(v)
}


dend_heights = function(x) {
    if(is.null(x)) return(0)

    if(inherits(x, "dendrogram")) {
        attr(x, "height")
    } else {
        sapply(x, function(y) attr(y, "height"))
    }
}
