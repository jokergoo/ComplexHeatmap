
pdf(NULL)

df = data.frame(type = c("a", "a", "a", "b", "b", "b"))
ha = HeatmapAnnotation(df = df)
draw(ha, 1:6)
draw(ha, 6:1)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")))
draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, col = list(type = c("a" =  "red", "b" = "blue")), which = "column")
draw(ha, 1:6)


value = 1:6
anno_points = function(index) {
	n = length(index)
	x = (seq_len(n) - 0.5) / n
	y  = (value - min(value))/(max(value) - min(value))
	grid.points(x, y, default.units = "npc")
}
ha = HeatmapAnnotation(points = anno_points)
draw(ha, 1:6)

ha = HeatmapAnnotation(df = df, points = anno_points)
draw(ha, 1:6)


ha = HeatmapAnnotation(df = df, points = anno_points, which = "column")
draw(ha, 1:6)

dev.off()
