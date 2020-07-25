
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

# == title
# Adjust the Positions of nodes/leaves in the Dendrogram
#
# == param
# -dend A `dendrogram` object.
# -leaf_pos A vector of positions of leaves. The value can also be a `grid::unit` object.
#
# == detail
# The positions of nodes stored as ``x`` attribute are recalculated based on the new positions of leaves.
# 
# By default, the position of leaves are at 0.5, 1.5, ..., n-0.5.
#
# == example
# m = matrix(rnorm(100), 10)
# dend = as.dendrogram(hclust(dist(m)))
# dend = adjust_dend_by_x(dend, sort(runif(10)))
# str(dend)
# dend = adjust_dend_by_x(dend, unit(1:10, "cm"))
# str(dend)
adjust_dend_by_x = function(dend, leaf_pos = 1:nobs(dend)-0.5) {
    n = nobs(dend)

    if(length(leaf_pos) != n) {
        stop_wrap("`leaf_pos` should be a vector with same length as `dend`.")
    }

    dend_order = order.dendrogram(dend)
    leaves_pos = leaf_pos
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


# == title
# Grob for Dendrogram
#
# == param
# -dend A `dendrogram` object.
# -facing Facing of the dendrogram.
# -order If it is set to ``reverse``, the first leaf is put on the right if the dendrogram
#        is horizontal and it is put on the top if the dendrogram is vertical.
# -gp Graphic parameters for the dendrogram segments. If any of ``col``, ``lwd`` or ``lty`` is set
#     in the ``edgePar`` attribute of a node, the corresponding value defined in ``gp`` will be
#     overwritten for this node, so ``gp`` is like global graphic parameters for dendrogram segments.
#
# == details
# If ``dend`` has not been processed by `adjust_dend_by_x`, internally `adjust_dend_by_x` is called
# to add ``x`` attributes to each node/leaf.
#
# == value
# A `grob` object which is contructed by `grid::segmentsGrob`.
#
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
        gb = segmentsGrob(lt$x0, lt$y0, lt$x1, lt$y1, gp = gpar(lwd = lt$lwd, lty = lt$lty, col = lt$col),
            default.units = "native")
    } else if(facing %in% c("left", "right")) {
        gb = segmentsGrob(lt$y0, lt$x0, lt$y1, lt$x1, gp = gpar(lwd = lt$lwd, lty = lt$lty, col = lt$col),
            default.units = "native")
    }
    gb$facing = facing
    return(gb)
}

# == title
# Draw the Dendrogram
#
# == param
# -dend A `dendrogram` object.
# -... Pass to `dendrogramGrob`.
# -test Is it in test mode? If it is in test mode, a viewport is created by calculating proper xlim and ylim.
#
# == detail
# `grid.dendrogram` supports drawing dendrograms with self-defind leaf positions. The positions
# of leaves can be defined by `adjust_dend_by_x`. Also the dendrogram can be customized by setting
# the ``edgePar`` attribute for each node (basically for controlling the style of segments), e.g.
# by `dendextend::color_branches`.
# 
# To draw the dendrogram, a viewport should be firstly created. `dend_xy` can be used to get the 
# positions of leaves and height of the dendrogram.
#
# == example
# m = matrix(rnorm(100), 10)
# dend = as.dendrogram(hclust(dist(m)))
# grid.newpage()
# pushViewport(viewport(xscale = c(0, 10.5), yscale = c(0, dend_heights(dend)), 
#     width = 0.9, height = 0.9))
# grid.dendrogram(dend)
# popViewport()
#
# grid.dendrogram(dend, test = TRUE)
#
# require(dendextend)
# dend = color_branches(dend, k = 2)
# dend = adjust_dend_by_x(dend, unit(sort(runif(10)*10), "cm"))
# grid.dendrogram(dend, test = TRUE)
grid.dendrogram = function(dend, ..., test = FALSE) {
    gb = dendrogramGrob(dend, ...)
    if(test) {
        h = dend_heights(dend)
        n = nobs(dend)
        grid.newpage()
        if(gb$facing %in% c("top", "bottom")) {
           pushViewport(viewport(xscale = c(-0.5, n + 0.5), yscale = c(-h*0.05, h*1.05), 
                width = unit(1, "npc") - unit(4, "cm"), 
                height = unit(1, "npc") - unit(4, "cm")))
        } else {
            pushViewport(viewport(yscale = c(-0.5, n + 0.5), xscale = c(-h*0.05, h*1.05), 
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

# == title
# Merge Dendrograms
# 
# == param
# -x The parent dendrogram.
# -y The children dendrograms. They are connected to the leaves of the parent dendrogram.
#           So the length of ``y`` should be as same as the number of leaves of the parent dendrogram.
# -only_parent Whether only returns the parent dendrogram where the height and node positions have
#              been adjusted by children dendrograms.
# -... Other arguments.
#
# == details
# Do not retrieve the order of the merged dendrogram. It is not reliable.
#
# == example
# m1 = matrix(rnorm(100), nr = 10)
# m2 = matrix(rnorm(80), nr = 8)
# m3 = matrix(rnorm(50), nr = 5)
# dend1 = as.dendrogram(hclust(dist(m1)))
# dend2 = as.dendrogram(hclust(dist(m2)))
# dend3 = as.dendrogram(hclust(dist(m3)))
# dend_p = as.dendrogram(hclust(dist(rbind(colMeans(m1), colMeans(m2), colMeans(m3)))))
# dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3))
# grid.dendrogram(dend_m, test = TRUE)
#
# dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3), only_parent = TRUE)
# grid.dendrogram(dend_m, test = TRUE)
#
# require(dendextend)
# dend1 = color_branches(dend1, k = 1, col = "red")
# dend2 = color_branches(dend2, k = 1, col = "blue")
# dend3 = color_branches(dend3, k = 1, col = "green")
# dend_p = color_branches(dend_p, k = 1, col = "orange")
# dend_m = merge_dendrogram(dend_p, list(dend1, dend2, dend3))
# grid.dendrogram(dend_m, test = TRUE)
merge_dendrogram = function(x, y, only_parent = FALSE, ...) {
    parent = x
    children = y

    n = nobs(parent)
    if(n != length(children)) {
        stop_wrap("Number of children dendrograms should be same as leaves in parent.")
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
    od_children = lapply(children, function(x) rank(order.dendrogram(x)))

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
    env = new.env()
    env$i = 0
    dendrapply(x, function(node) {
        if(is.leaf(node)) {
            env$i = env$i + 1
            node[[1]] = value[ env$i ]
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


# == title
# Height of the Dendrograms
#
# == param
# -x a `dendrogram` object or a list of `dendrogram` objects.
#
dend_heights = function(x) {
    if(is.null(x)) return(0)

    if(inherits(x, "dendrogram")) {
        attr(x, "height")
    } else {
        sapply(x, function(y) attr(y, "height"))
    }
}

# == title
# Coordinates of the Dendrogram
#
# == param
# -dend a `dendrogram` object.
#
# == detail
# ``dend`` will be processed by `adjust_dend_by_x` if it is processed yet.
#
# == value
# A list of leave positions (``x``) and dendrogram height (``y``).
#
# == example
# m = matrix(rnorm(100), 10)
# dend1 = as.dendrogram(hclust(dist(m)))
# dend_xy(dend1)
#
# dend1 = adjust_dend_by_x(dend1, sort(runif(10)))
# dend_xy(dend1)
#
# dend1 = adjust_dend_by_x(dend1, unit(1:10, "cm"))
# dend_xy(dend1)
dend_xy = function(dend) {
    if(is.null(attr(dend, "x"))) {
        dend = adjust_dend_by_x(dend)
    }
    env = new.env()
    env$lt = list()
    dendrapply(dend, function(d) {
        if(is.leaf(d))
            env$lt = c(env$lt, list(attr(d, "x")))
    })
    x = env$lt
    if(inherits(x[[1]], "unit")) {
        x = do.call("unit.c", x)
    } else {
        x = unlist(x)
    }
    return(list(x = x,
                y = c(0, dend_heights(dend))))
}


# == title
# Cluster within and between Groups
#
# == param
# -mat A matrix where clustering is applied on columns.
# -factor A categorical vector.
#
# == details
# The clustering is firstly applied in each group, then clustering is applied
# to group means. The within-group dendrograms and between-group dendrogram
# are finally connected by `merge_dendrogram`.
#
# In the final dendrogram, the within group dendrograms are enforced to be 
# flat lines to emphasize that the within group dendrograms have no sense to 
# compare to between-group dendrogram.
#
# == value
# A `dendrogram` object. The order of columns can be retrieved by `stats::order.dendrogram`.
#
# == example
# m = matrix(rnorm(120), nc = 12)
# colnames(m) = letters[1:12]
# fa = rep(c("a", "b", "c"), times = c(2, 4, 6))
# dend = cluster_within_group(m, fa)
# grid.dendrogram(dend, test = TRUE)
cluster_within_group = function(mat, factor) {

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
        } else if(ncol(m) > 1) {
            hc1 = hclust(dist(t(m)))
            dend_list[[le]] = as.dendrogram(hc1)
            order_list[[le]] = which(factor == le)[order.dendrogram(dend_list[[le]])]
            order.dendrogram(dend_list[[le]]) = order_list[[le]]
        }
    }

    parent = as.dendrogram(hclust(dist(t(sapply(order_list, function(x) rowMeans(mat[, x, drop = FALSE]))))))
    dend_list = lapply(dend_list, function(dend) dendrapply(dend, function(node) {
        attr(node, "height") = 0
        node
    }))
    dend = merge_dendrogram(parent, dend_list)
    order.dendrogram(dend) = unlist(order_list[order.dendrogram(parent)])
    return(dend)
}
