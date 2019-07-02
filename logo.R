

logo = readLines(textConnection("
oooooo                  oooooo  oo          oo  oo
oooooo                  oooooo  oo          oo  oo
oo      oooooo  oo  oo  oo  oo  oo  oooooo  oooooo
oo      oooooo  oooooo  oo  oo  oo  oooooo  oooooo
oo      oo  oo  oooooo  oooo    oo  oo  oo    oo
oo      oo  oo  oooooo  oooo    oo  oo  oo    oo
oooooo  oooooo  oo  oo  oo      oo  oooo    oo  oo
oooooo  oooooo  oo  oo  oo      oo  oooo    oo  oo

oo  oo                    oo                    oooooo
oo  oo                    oo                    oooooo
oo  oo  oooooo    oooo  oooooo  oo  oo  oooooo  oo  oo
oo  oo  oooooo    oooo  oooooo  oooooo  oooooo  oo  oo
oooooo  oo  oo  oo  oo    oo    oooooo  oo  oo  oooo
oooooo  oo  oo  oo  oo    oo    oooooo  oo  oo  oooo
oo  oo  oooo    oooooo    oo    oo  oo  oooo    oo
oo  oo  oooo    oooooo    oo    oo  oo  oooo    oo
"))

logo = strsplit(logo, "")

mat1 = matrix(0, nrow = 8, ncol = max(sapply(logo[2:9], length)))
for(i in 2:9) {
	mat1[i - 1, which(!grepl("^\\s*$", logo[[i]]))] = 1
}
mat1 = cbind(matrix(0, nrow = nrow(mat1), ncol = 2), mat1)

mat2 = matrix(0, nrow = 8, ncol = max(sapply(logo[11:18], length)))
for(i in 11:18) {
	mat2[i - 10, which(!grepl("^\\s*$", logo[[i]]))] = 1
}

mat = cbind(mat1, matrix(0, nrow = nrow(mat1), ncol = 4), mat2)

if(ncol(mat1) > ncol(mat2)) {
	mat2 = cbind(mat2, matrix(0, nrow = nrow(mat2), ncol = ncol(mat1) - ncol(mat2)))
} else {
	mat1 = cbind(mat1, matrix(0, nrow = nrow(mat1), ncol = ncol(mat2) - ncol(mat1)))
}

mat = rbind(mat1, matrix(0, nrow = 2, ncol = ncol(mat1)), mat2)

mat = rbind(matrix(0, nrow = 30, ncol = ncol(mat1)),
	        mat,
	        matrix(0, nrow = 30, ncol = ncol(mat1)))

mat = cbind(matrix(0, nrow = nrow(mat), ncol = 10),
	        mat,
	        matrix(0, nrow = nrow(mat), ncol = 10))

mat[nrow(mat) - 27, 9:(ncol(mat) - 8)] = 1
mat[nrow(mat) - 26, 9:(ncol(mat) - 8)] = 1
mat[28, 9:(ncol(mat) - 8)] = 1
mat[27, 9:(ncol(mat) - 8)] = 1



library(ComplexHeatmap)
library(circlize)
col_fun = function(x) {
	n = length(x)
	col = ifelse(x == 1, add_transparency("#4DAF4A", runif(n, min = 0, max = 0.3)),
		                 rand_color(n, luminosity = "light", transparency = 0.8))
}
attr(col_fun, "breaks") = c(0, 1)
ht = Heatmap(mat, name = "foo", rect_gp = gpar(col = "white", lwd = 0.5), cluster_rows = FALSE, cluster_columns = FALSE,
	col = col_fun, show_heatmap_legend = FALSE)
g = grid.grabExpr(draw(ht, padding = unit(c(0, 0, 0, 0), "mm")))

grid.newpage()
pushViewport(viewport(xscale = c(0, 2), yscale = c(0, 2), width = unit(0.9, "snpc"), height = unit(0.9, "snpc")))
grid.polygon(cos(0:5 * pi/3 + pi/6)*1 + 1,
     sin(0:5 * pi/3 + pi/6)*1 + 1, default.units = "native",
     gp = gpar(col = "#4DAF4A", lwd = 6))
height = 1
pushViewport(viewport(x = 0.5, y = 0.5, height = height, width = height*ncol(mat)/nrow(mat)))
grid.draw(g)
popViewport()
grid.polygon(cos(0:5 * pi/3 + pi/6)*1 + 1,
     sin(0:5 * pi/3 + pi/6)*1 + 1, default.units = "native",
     gp = gpar(col = "#4DAF4A", lwd = 8))
popViewport()

library(gridgeometry)
A = g
B = polygonGrid(cos(0:5 * pi/3 + pi/6)*1 + 1,
     sin(0:5 * pi/3 + pi/6)*1 + 1, default.units = "native",
     gp = gpar(col = "#4DAF4A", lwd = 8))
