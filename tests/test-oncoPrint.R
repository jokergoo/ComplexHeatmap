library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

mat = read.table(textConnection(
"s1,s2,s3
g1,snv;indel,snv,indel
g2,,snv;indel,snv
g3,snv,,indel;snv"), row.names = 1, header = TRUE, sep = ",", stringsAsFactors = FALSE)
mat = as.matrix(mat)

get_type_fun = function(x) strsplit(x, ";")[[1]]

alter_fun = list(
    snv = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.9, 
        gp = gpar(fill = col["snv"], col = NA)),
    indel = function(x, y, w, h) grid.rect(x, y, w*0.9, h*0.4, 
        gp = gpar(fill = col["indel"], col = NA))
)

col = c(snv = "red", indel = "blue")
ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col)
draw(ht)

## turn off row names while turn on column names
ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col, 
    show_column_names = TRUE, show_row_names = FALSE, show_pct = FALSE)
draw(ht)

ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col, pct_side = "right", 
    row_names_side = "left")
draw(ht)

ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col,
    top_annotation = HeatmapAnnotation(column_barplot = anno_oncoprint_barplot())
)
draw(ht)

ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col,
    top_annotation = HeatmapAnnotation(
    	column_barplot = anno_oncoprint_barplot(),
    	foo = 1:3,
    	annotation_name_side = "left")
)
draw(ht)

ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col,
    top_annotation = HeatmapAnnotation(
    	cbar = anno_oncoprint_barplot(),
    	foo1 = 1:3,
    	annotation_name_side = "left"),
    left_annotation = rowAnnotation(foo2 = 1:3),
    right_annotation = rowAnnotation(cbar = anno_oncoprint_barplot(), foo3 = 1:3),
)
draw(ht)


ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col,
    top_annotation = HeatmapAnnotation(
        cbar = anno_oncoprint_barplot(border = TRUE),
        foo1 = 1:3,
        annotation_name_side = "left"),
    left_annotation = rowAnnotation(foo2 = 1:3),
    right_annotation = rowAnnotation(
        cbar = anno_oncoprint_barplot(border = TRUE), 
        foo3 = 1:3),
)
draw(ht)

ht = oncoPrint(mat, get_type = get_type_fun,
    alter_fun = alter_fun, col = col,
    right_annotation = rowAnnotation(rbar = anno_oncoprint_barplot(axis_param = list(side = "bottom", labels_rot = 90)))
)
draw(ht)

