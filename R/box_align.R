
# == title
# Adjust positions of rectanglar shapes
#
# == param
# -start position which corresponds to the start (bottom or left) of the rectangle-shapes.
# -end position which corresponds to the end (top or right) of the rectanglar shapes.
# -range data ranges (the minimal and maximal values)
# -plot Whether plot the correspondance between the original positions and the adjusted positions. Only for testing.
#
# == details
# This is an improved version of the `circlize::smartAlign`.
#
# It adjusts the positions of the rectangular shapes to make them do not overlap
#
# == example
# range = c(0, 10)
# pos1 = rbind(c(1, 2), c(5, 7))
# smartAlign2(pos1, range = range, plot = TRUE)
#
# range = c(0, 10)
# pos1 = rbind(c(-0.5, 2), c(5, 7))
# smartAlign2(pos1, range = range, plot = TRUE)
#
# pos1 = rbind(c(-1, 2), c(3, 4), c(5, 6), c(7, 11))
# pos1 = pos1 + runif(length(pos1), max = 0.3, min = -0.3)
# omfrow = par("mfrow")
# par(mfrow = c(3, 3))
# for(i in 1:9) {
#     ind = sample(4, 4)
#     smartAlign2(pos1[ind, ], range = range, plot = TRUE)
# }
# par(mfrow = omfrow)
#
# pos1 = rbind(c(3, 6), c(4, 7))
# smartAlign2(pos1, range = range, plot = TRUE)
#
# pos1 = rbind(c(1, 8), c(3, 10))
# smartAlign2(pos1, range = range, plot = TRUE)
#
smartAlign2 = function(start, end, range, plot = FALSE) {
    if(missing(end)) {
        end = start[, 2]
        start = start[, 1]    
    }

    if(missing(range)) {
        range = range(c(start, end))
    }


    if(0) {
        qqcat("start <- c(@{toString(start)})\n")
        qqcat("end <- c(@{toString(end)})\n")
    }

    # sum of box heights exceeds range
    if(sum(end - start) > range[2] - range[1]) {

        mid = (start + end)/2
        od = order(mid)
        rk = rank(mid, ties.method = "random")
        h = end[od] - start[od]
        n = length(start)
        
        mid_diff = numeric(n); mid_diff[1] = 0
        mid_diff[2:n] = h[2:n]/2 + h[1:(n-1)]/2
        mid_radius = sum(h) - h[n]/2 - h[1]/2

        a_1 = range[1] + h[1]/2
        a_n = range[2] - h[n]/2
        
        a = a_1 + cumsum(mid_diff)/mid_radius * (a_n - a_1)
        new_start = a - h/2
        new_end = a + h/2

        df = data.frame(start = new_start, end = new_end)
        return(df[rk, , drop = FALSE])
    }

    ba = BoxArrange(start, end, range)
    if(plot) ba$plot()
    ba$box_pos()
}

box_overlap = function(x1, x2) {
    start1 = x1$start
    end1 = x1$end
    start2 = x2$start
    end2 = x2$end

    if(end1 < start2) {
        FALSE
    } else if(end2 < start1) {
        FALSE
    } else {
        TRUE
    }
}

Box = setRefClass("Box",
    fields = list(
        start = "numeric",
        end = "numeric",
        new_start = "numeric",
        new_end = "numeric",
        height = "numeric",
        mid = "numeric"
    )
)

Box$methods(initialize = function(start, end) {
    start <<- start
    end <<- end
    new_start <<- start
    new_end <<- end
    height <<- end - start
    mid <<- (start + end)/2
})

BoxCluster = setRefClass("BoxCluster",
    fields = list(
        start = "numeric",
        end = "numeric",
        height = "numeric",
        mid = "numeric",
        boxes = "list"
    )
)

BoxCluster$methods(add_box = function(box) {
    if(inherits(box, "Box")) {
        boxes <<- c(.self$boxes, list(box))
    } else {
        boxes <<- c(.self$boxes, box$boxes)
    }
    s = min(sapply(.self$boxes, function(b) b$start))
    e = max(sapply(.self$boxes, function(b) b$end))
    mid <<- (s + e)/2
    height <<- e - s
    start <<- s
    end <<- e
    
    invisible(NULL)
})

BoxCluster$methods(flatten = function(range) {

    s = min(sapply(.self$boxes, function(b) b$start))
    e = max(sapply(.self$boxes, function(b) b$end))

    mid <<- (s + e)/2
    height <<- sum(sapply(.self$boxes, function(b) b$height))
    s2 = .self$mid - .self$height/2
    e2 = .self$mid + .self$height/2

    if(s2 < range[1]) {
        s2 = range[1]
        e2 = s2 + height
    } else if(e2 > range[2]) {
        e2 = range[2]
        s2 = e2 - height
    }

    start <<- s2
    end <<- e2

    .self$update_box_new_pos()
})

BoxCluster$methods(update_box_new_pos = function() {
    h = sapply(.self$boxes, function(b) b$height)
    new_s = .self$start + cumsum(c(0, h[-length(h)]))
    new_e = .self$start + cumsum(h)

    bb = .self$boxes
    for(i in seq_along(bb)) {
        bb[[i]]$new_start = new_s[i]
        bb[[i]]$new_end = new_e[i]
    }
    boxes <<- bb
})

BoxCluster$methods(move = function(x) {
    start <<- .self$start + x
    end <<- .self$end + x
    mid <<- .self$mid + x

    .self$update_box_new_pos()

    invisible(NULL)
})

BoxCluster$methods(middle = function(original = TRUE) {
    if(original) {
        s = min(sapply(.self$boxes, function(b) b$start))
        e = max(sapply(.self$boxes, function(b) b$end))
        (s + e)/2
    } else {
        .self$mid
    }
})

BoxCluster$methods(show = function(x) {
    n = length(.self$boxes)
    qqcat("@{n} box@{ifelse(n > 1, 'es', '')}\n")
    qqcat("start: @{.self$start}\n")
    qqcat("end: @{.self$end}\n")
    qqcat("height: @{.self$height}\n")
})

BoxArrange = setRefClass("BoxArrange",
    fields = list(
        box_clusters = "list",
        range = "numeric",
        rank = "integer",
        debug = "logical"
    )
)

BoxArrange$methods(initialize = function(start, end, range = range(c(start, end)), debug = FALSE) {

    if(sum(end - start) > range[2] - range[1]) {
        stop()
    }
    
    boxes = list()
    for(i in seq_along(start)) {
        boxes[[i]] = Box(start = start[i], end = end[i])
    }
    mid = sapply(boxes, function(b) (b$start + b$end)/2)
    boxes = boxes[order(mid)]

    # initialize
    cl = list()
    cl[[1]] = BoxCluster()
    cl[[1]]$add_box(boxes[[1]])
    i_cluster = 1
    for(i in seq_along(boxes)[-1]) {
        if(box_overlap(cl[[i_cluster]], boxes[[i]])) {
            cl[[i_cluster]]$add_box(boxes[[i]])
        } else {
            i_cluster = i_cluster + 1
            cl[[i_cluster]] = BoxCluster()
            cl[[i_cluster]]$add_box(boxes[[i]])
        }
    }
    for(i in seq_along(cl)) {
        cl[[i]]$flatten(range)
    }
    box_clusters <<- cl
    range <<- range
    rank <<- rank(mid, ties.method = "random")
    debug <<- debug

    if(debug) {
        .self$plot(main = "initialize")
    }

    .self$merge()
    if(debug) {
        .self$plot(main = "merge after initialization")
    }

    .self$adjust_to_range()
    if(debug) {
        .self$plot(main = "adjust to ranges")
    }

    .self$merge()
    if(debug) {
        .self$plot(main = "merge after adjusting to ranges")
    }
})

# recursive merge
BoxArrange$methods(merge = function() {
    cl = .self$box_clusters
    while(1) {
        merged = FALSE
        n_cluster = length(cl)
        for(i in seq_len(n_cluster)[-1]) {
            if(box_overlap(cl[[i]], cl[[i-1]])) {
                cl[[i-1]]$add_box(cl[[i]])
                cl[[i]] = cl[[i-1]]
                cl[[i-1]] = NA
                merged = TRUE
                if(.self$debug) qqcat("cluster @{i-1} and @{i} are merged\n")
            }
        }
        if(!merged) {
            break
        }
        cl = cl[!sapply(cl, identical, NA)]
        for(i in seq_along(cl)) {
            cl[[i]]$flatten(.self$range)
        }
    }
    box_clusters <<- cl
    invisible(NULL)
})

## arrange box clusters, assume sum(height) < range[2] - range[1]
BoxArrange$methods(adjust_to_range = function() {
    cl = .self$box_clusters
    for(i in seq_along(cl)) {
        if(cl[[i]]$start < range[1]) {
            cl[[i]]$move(range[1] - cl[[i]]$start)
        }
        if(cl[[i]]$end > range[2]) {
            cl[[i]]$move(range[2] - cl[[i]]$end)
        }
    }
    box_clusters <<- cl
    .self$merge()
    invisible(NULL)
})


BoxArrange$methods(n_cluster = function() {
    length(.self$box_clusters)
})


BoxArrange$methods(box_pos = function(new = TRUE) {
    if(new) {
        pos_lt = lapply(.self$box_clusters, function(x) {
            do.call(rbind, lapply(x$boxes, function(b) {
                c(b$new_start, b$new_end)
            }))
        })
    } else {
        pos_lt = lapply(.self$box_clusters, function(x) {
            do.call(rbind, lapply(x$boxes, function(b) {
                c(b$start, b$end)
            }))
        })
    }
    pos = do.call(rbind, pos_lt)
    pos[.self$rank, , drop = FALSE]
})

## now we can arrange clusters

BoxArrange$methods(plot = function(main = "") {

    pos1_lt = lapply(.self$box_clusters, function(x) {
        do.call(rbind, lapply(x$boxes, function(b) {
            c(b$start, b$end)
        }))
    })

    pos2_lt = lapply(.self$box_clusters, function(x) {
        do.call(rbind, lapply(x$boxes, function(b) {
            c(b$new_start, b$new_end)
        }))
    })

    n = length(pos1_lt)

    oxpd = par("xpd")
    par(xpd = NA)
    graphics::plot(NULL, xlim = c(0, 4), ylim = range(c(.self$range, unlist(pos1_lt), unlist(pos2_lt))), 
        ann = FALSE, axes = FALSE)
    for(i in seq_len(n)) {
        pos1 = pos1_lt[[i]]
        pos2 = pos2_lt[[i]]

        col = add_transparency(i, 0.5)
        rect(0.5, pos1[, 1], 1.5, pos1[, 2], col = col)
        rect(2.5, pos2[, 1], 3.5, pos2[, 2], col = col)
        segments(1.5, rowMeans(pos1), 2.5, rowMeans(pos2), col = i)

        start = .self$box_clusters[[i]]$start
        end = .self$box_clusters[[i]]$end
        rect(2.5, start, 3.5, end, border = i, col = NA, lwd = 2)
    }

    text(1, -0.02, "original", adj = c(0.5, 1))
    text(3, -0.02, "adjusted", adj = c(0.5, 1))
    rect(0, .self$range[1], 4, .self$range[2], col = NA, border = "black")
    title(qq("@{main}, @{n} clusters"))
    par(xpd = oxpd)

    readline(prompt = "enter to continue")
})

