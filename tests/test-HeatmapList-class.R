
library(circlize)
library(grid)
library(RColorBrewer)

source("Heatmap-class.R")
source("HeatmapList-class.R")
source("ColorMapping-class.R")
source("utils.R")

mat = matrix(rnorm(40, 2), 4, 10)
mat = rbind(mat, matrix(rnorm(40, -2), 4, 10))
rownames(mat) = letters[1:8]
colnames(mat) = letters[1:10]

Heatmap(mat)
