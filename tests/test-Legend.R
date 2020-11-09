library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

if(!exists("random_str")) {
	random_str = ComplexHeatmap:::random_str
}

lgd = Legend(at = 1:6, legend_gp = gpar(fill = 1:6))
draw(lgd, test = "default discrete legends style")

lgd = Legend(labels = 1:6, legend_gp = gpar(fill = 1:6))
draw(lgd, test = "only specify labels with no at")


lgd = Legend(labels = month.name[1:6], title = "foo", legend_gp = gpar(fill = 1:6))
draw(lgd, test = "add labels and title")

lgd = Legend(labels = month.name[1:6], title = "foo", legend_gp = gpar(fill = 1:6),
	title_position = "lefttop")
draw(lgd, test = "title put in the lefttop")

lgd = Legend(labels = month.name[1:6], title = "foo", legend_gp = gpar(fill = 1:6),
	title_position = "lefttop-rot")
draw(lgd, test = "title put in the lefttop-rot")

lgd = Legend(labels = month.name[1:6], title = "foo", legend_gp = gpar(fill = 1:6),
	title_position = "leftcenter-rot")
draw(lgd, test = "title put in the leftcenter-rot")

lgd = Legend(labels = 1:6, title = "fooooooo", legend_gp = gpar(fill = 1:6))
draw(lgd, test = "title is longer than the legend body")

lgd = Legend(at = 1:6, legend_gp = gpar(fill = 1:6), grid_height = unit(1, "cm"), 
	title = "foo", grid_width = unit(5, "mm"))
draw(lgd, test = "grid size")

lgd = Legend(labels = month.name[1:6], legend_gp = gpar(fill = 1:6), title = "foo", 
	labels_gp = gpar(col = "red", fontsize = 14))
draw(lgd, test = "labels_gp")

lgd = Legend(labels = month.name[1:6], legend_gp = gpar(fill = 1:6), title = "foo", 
	title_gp = gpar(col = "red", fontsize = 14))
draw(lgd, test = "title_gp")

lgd = Legend(labels = month.name[1:6], legend_gp = gpar(fill = 1:6), title = "foo", 
	border = "red")
draw(lgd, test = "legend border")

lgd = Legend(labels = month.name[1:10], legend_gp = gpar(fill = 1:10), title = "foo", 
	ncol = 3)
draw(lgd, test = "in 3 columns")

lgd = Legend(labels = month.name[1:10], legend_gp = gpar(fill = 1:10), title = "foo", 
	ncol = 3, title_position = "topcenter")
draw(lgd, test = "in 3 columns, title in the center")

lgd = Legend(labels = month.name[1:10], legend_gp = gpar(fill = 1:10), title = "foo", 
	ncol = 3, by_row = TRUE)
draw(lgd, test = "in 3 columns and by rows")

lgd = Legend(labels = month.name[1:10], legend_gp = gpar(fill = 1:10), title = "foo", 
	ncol = 3, gap = unit(1, "cm"))
draw(lgd, test = "in 3 columns with gap between columns")

lgd = Legend(labels = month.name[1:10], legend_gp = gpar(fill = 1:10), title = "foo", 
	nrow = 3)
draw(lgd, test = "in 3 rows")

lgd = Legend(labels = month.name[1:6], legend_gp = gpar(fill = 1:6), title = "foooooo", 
	nrow = 1, title_position = "lefttop")
draw(lgd, test = "1 row and title is on the left")

lgd = Legend(labels = month.name[1:6], legend_gp = gpar(fill = 1:6), title = "foooooo", 
	nrow = 1, title_position = "lefttop-rot")
draw(lgd, test = "1 row and title is on the left, 90 rotation")

lgd = Legend(labels = month.name[1:6], legend_gp = gpar(fill = 1:6), title = "foooooo", 
	nrow = 1, title_position = "leftcenter")
draw(lgd, test = "1 row and title is on the left, 90 rotation")

lgd = Legend(labels = month.name[1:6], title = "foo", type = "points", pch = 1:6, 
	legend_gp = gpar(col = 1:6), background = "red")
draw(lgd, test = "points as legends")

lgd = Legend(labels = month.name[1:6], title = "foo", type = "points", pch = letters[1:6], 
	legend_gp = gpar(col = 1:6), background = "white")
draw(lgd, test = "letters as legends")

lgd = Legend(labels = month.name[1:6], title = "foo", type = "lines", 
	legend_gp = gpar(col = 1:6, lty = 1:6))
draw(lgd, test = "lines as legends")

###### vertical continous legend #######
col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
lgd = Legend(col_fun = col_fun, title = "foo")
draw(lgd, test = "only col_fun")

lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.25, 0.5, 0.75, 1))
draw(lgd, test = "with at")

lgd = Legend(col_fun = col_fun, title = "foo", at = rev(c(0, 0.25, 0.5, 0.75, 1)))
draw(lgd, test = "with at")


lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.5, 1), labels = c("low", "median", "high"))
draw(lgd, test = "with labels")

lgd = Legend(col_fun = col_fun, title = "foo", legend_height = unit(6, "cm"))
draw(lgd, test = "set legend_height")

lgd = Legend(col_fun = col_fun, title = "foo", labels_gp = gpar(col = "red"))
draw(lgd, test = "set label color")

lgd = Legend(col_fun = col_fun, title = "foo", border = "red")
draw(lgd, test = "legend border")

lgd = Legend(col_fun = col_fun, title = "foooooooo", title_position = "lefttop-rot")
draw(lgd, test = "lefttop rot title")

lgd = Legend(col_fun = col_fun, title = "foooooooo", title_position = "leftcenter-rot")
draw(lgd, test = "leftcenter top title")


lgd = Legend(col_fun = col_fun, title = "foo", title_position = "lefttop", direction = "horizontal")
draw(lgd, test = "lefttop title")

###### horizontal continous legend #######
col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
lgd = Legend(col_fun = col_fun, title = "foo", direction = "horizontal")
draw(lgd, test = "only col_fun")

lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.25, 0.5, 0.75, 1), direction = "horizontal")
draw(lgd, test = "with at")

lgd = Legend(col_fun = col_fun, title = "foo", at = rev(c(0, 0.25, 0.5, 0.75, 1)), direction = "horizontal")
draw(lgd, test = "with at")

lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.5, 1), labels = c("low", "median", "high"),
	direction = "horizontal")
draw(lgd, test = "with labels")

lgd = Legend(col_fun = col_fun, title = "foo", legend_width = unit(6, "cm"), direction = "horizontal")
draw(lgd, test = "set legend_width")

lgd = Legend(col_fun = col_fun, title = "foo", labels_gp = gpar(col = "red"), direction = "horizontal")
draw(lgd, test = "set label color")

lgd = Legend(col_fun = col_fun, title = "foo", border = "red", direction = "horizontal")
draw(lgd, test = "legend border")

lgd = Legend(col_fun = col_fun, title = "foooooooo", direction = "horizontal", 
	title_position = "topcenter")
draw(lgd, test = "topcenter title")

lgd = Legend(col_fun = col_fun, title = "foooooooo", direction = "horizontal", 
	title_position = "lefttop")
draw(lgd, test = "lefttop title")

lgd = Legend(col_fun = col_fun, title = "foooooooo", direction = "horizontal", 
	title_position = "leftcenter")
draw(lgd, test = "leftcenter title")


###### pack legend
lgd1 = Legend(at = 1:6, legend_gp = gpar(fill = 1:6), title = "legend1")
lgd2 = Legend(col_fun = col_fun, title = "legend2", at = c(0, 0.25, 0.5, 0.75, 1))

pd = packLegend(lgd1, lgd2)
draw(pd, test = "two legends")

pd = packLegend(list = list(lgd1, lgd2))
draw(pd, test = "two legends specified as a list")

pd = packLegend(lgd1, lgd2, direction = "horizontal")
draw(pd, test = "two legends packed horizontally")

lgd3 = Legend(at = 1:6, legend_gp = gpar(fill = 1:6), title = "legend1")
lgd4 = Legend(col_fun = col_fun, title = "legend2", at = c(0, 0.25, 0.5, 0.75, 1), direction = "horizontal")
pd = packLegend(lgd3, lgd4)
draw(pd, test = "two legends with different directions")
pd = packLegend(lgd3, lgd4, direction = "horizontal")
draw(pd, test = "two legends with different directions")

pd = packLegend(lgd1, lgd2, lgd1, lgd2)
draw(pd, test = "many legends with same legends")

lgd3 = Legend(at = 1:6, legend_gp = gpar(fill = 1:6), title = "legend1")
lgd4 = Legend(col_fun = col_fun, title = "legend2", at = c(0, 0.25, 0.5, 0.75, 1))
pd = packLegend(lgd1, lgd2, lgd3, lgd4)
draw(pd, test = "many legends with all different legends")

pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2)
draw(pd, test = "many legends")

pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, max_height = unit(1, "npc"))
draw(pd, test = "many legends, max_height = unit(1, 'npc')")
## reduce the height of the interactive window and rerun draw()

pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, max_height = unit(10, "cm"))
draw(pd, test = "many legends, max_height = unit(10, 'cm')")

pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, max_height = unit(10, "cm"), gap = unit(1, "cm"))
draw(pd, test = "many legends, max_height = unit(10, 'cm'), with gap")

lgd_long = Legend(at = 1:50, legend_gp = gpar(fill = 1:50))
pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, lgd_long, max_height = unit(10, "cm"))
draw(pd, test = "many legends with a long one, max_height = unit(10, 'cm')")

lgd1 = Legend(at = 1:6, legend_gp = gpar(fill = 1:6), title = "legend1",
	nr = 1)
lgd2 = Legend(col_fun = col_fun, title = "legend2", at = c(0, 0.25, 0.5, 0.75, 1),
	direction = "horizontal")
pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, direction = "horizontal")
draw(pd, test = "many legends")

pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, max_width = unit(1, "npc"), direction = "horizontal")
draw(pd, test = "many legends, max_width = unit(1, 'npc')")
## reduce the height of the interactive window and rerun draw()

pd = packLegend(lgd1, lgd2, lgd1, lgd2, lgd1, lgd2, max_width = unit(10, "cm"), direction = "horizontal")
draw(pd, test = "many legends, max_width = unit(10, 'cm')")


####### unequal interval breaks
col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red"))
lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.15, 0.5, 0.9, 0.95, 1))
draw(lgd, test = "unequal interval breaks")
lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.3, 1), legend_height = unit(4, "cm"))
draw(lgd, test = "unequal interval breaks but not label position adjustment")

lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.15, 0.5, 0.9, 0.95, 1),
	direction = "horizontal")
draw(lgd, test = "unequal interval breaks")

lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.15, 0.5, 0.9, 0.95, 1),
	direction = "horizontal", title_position = "lefttop")
draw(lgd, test = "unequal interval breaks")


lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.15, 0.5, 0.9, 0.95, 1),
	direction = "horizontal", title_position = "lefttop", labels_rot = 90)
draw(lgd, test = "unequal interval breaks, label rot 90")

lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.5, 0.75, 1),
	labels = c("mininal", "q10", "median", "q75", "maximal"),
	direction = "horizontal", title_position = "lefttop")
draw(lgd, test = "unequal interval breaks with labels")


lgd = Legend(col_fun = col_fun, title = "foo", at = c(0, 0.1, 0.5, 0.75, 1),
	labels = c("mininal", "q10", "median", "q75", "maximal"),
	direction = "horizontal")
draw(lgd, test = "unequal interval breaks with labels")


col_fun = colorRamp2(c(0, 0.05, 0.1, 0.5, 1), c("green", "white", "red", "black", "blue"))
lgd = Legend(col_fun = col_fun, title = "foo", break_dist = 1:4)
draw(lgd, test = "unequal interval breaks")


#### position of legends to heatmaps ##
if(0) {
m = matrix(rnorm(100), 10)
rownames(m) = random_str(10, len = 20)
colnames(m) = random_str(10, len = 20)
Heatmap(m)
}


