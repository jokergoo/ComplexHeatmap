
if(0) {
##### test anno_richtext ####
anno = anno_text(month.name)

anno = anno_richtext(richtext_grob(month.name, box_gp = gpar(col = "red"), rot = 90, hjust = 1, y = unit(1, "npc")))
draw(anno, test = "month names")
anno = anno_richtext(richtext_grob(month.name, box_gp = gpar(col = "red"), hjust = 0, x = unit(0, "npc")), which = "row")
draw(anno, test = "month names")


reprex({
library(gridtext)
library(grid)
gb = richtext_grob(month.name, rot = 90, align_widths = FALSE)
convertHeight(grobHeight(gb), "mm")
gb = richtext_grob(month.name, rot = 90, align_widths = TRUE)
convertHeight(grobHeight(gb), "mm")

# only September
gb = richtext_grob(month.name[9], rot = 90, align_widths = FALSE)
convertHeight(grobHeight(gb), "mm")
})

m = matrix(rnorm(144), 12)
rownames(m) = month.name
colnames(m) = month.name

Heatmap(m, row_labels = richtext_grob(rownames(m), align_widths = TRUE, box_gp = gpar(col = "red"), x = 0, hjust = 0),
	column_labels = richtext_grob(colnames(m), box_gp = gpar(col = "blue"), y = 1, hjust = 1, rot = 90))



Heatmap(m)
}
