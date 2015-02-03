
library(circlize)
library(grid)
library(RColorBrewer)

source("Heatmap-class.R")
source("HeatmapList-class.R")
source("ColorMapping-class.R")
source("utils.R")
source("draw_s4.R")

mat = matrix(rnorm(40, 2), 4, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:8]
colnames(mat) = letters[1:10]

ht1 = Heatmap(mat)
ht2 = Heatmap(mat)

ht_list = ht1 + ht2

draw(ht_list)
draw(ht_list, heatmap_legend_side = "bottom")

ht_list$make_layout()
ht_list$draw()
