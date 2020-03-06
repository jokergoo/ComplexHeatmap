library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

m = matrix(rnorm(100), 10)

postscript("test.ps")
lgd = Legend(labels = c("a", "b", "c"))
draw(Heatmap(m), heatmap_legend_list = list(lgd))
dev.off()

check_pages = function() {
	lines = readLines("test.ps")
	print(lines[length(lines)-1])
	invisible(file.remove("test.ps"))
}

check_pages()

postscript("test.ps")
ha = HeatmapAnnotation(foo = 1:10, bar = anno_points(1:10))
Heatmap(m, top_annotation = ha)
dev.off()

check_pages()
