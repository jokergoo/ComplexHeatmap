
pdf(NULL)

df = data.frame(type = c("a", "a", "a", "b", "b", "b"))
ha = HeatmapAnnotation(df = df)
draw(ha, 1:6)
draw(ha, 6:1)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")))
draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")), which = "row")
draw(ha, 1:6)


value = 1:6
anno_points = function(index) {
	n = length(index)
	pushViewport(viewport(xscale = c(0.5, n+0.5), yscale = range(value)))
	grid.points(seq_along(index), value[index])
	upViewport()
}
ha = HeatmapAnnotation(points = anno_points)
draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, points = anno_points)
draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, points = anno_points, which = "row")
draw(ha, 1:6)

ha = HeatmapAnnotation(points = anno_simple(1:6, type = "p"))
draw(ha, 1:6)

ha = HeatmapAnnotation(histogram = anno_simple(1:6, type = "histogram"))
draw(ha, 1:6)

mat = matrix(rnorm(36), 6)
ha = HeatmapAnnotation(boxplot = anno_boxplot(mat))
draw(ha, 1:6)

dev.off()
