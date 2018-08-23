
ht_global_opt$verbose = FALSE
ht_global_opt$show_vp_border = FALSE

set.seed(123)
nr1 = 10; nr2 = 8; nr3 = 6
nc1 = 6; nc2 = 8; nc3 = 10
mat1 = cbind(rbind(matrix(rnorm(nr1*nc1, mean = 1,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc1, mean = 0,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc1, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc2, mean = 0,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc2, mean = 1,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc2, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc3, mean = 0.5, sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc3, mean = 0.5, sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc3, mean = 1,   sd = 0.5), nr = nr3))
   )

rownames(mat1) = paste0("row_1_", seq_len(nrow(mat1)))
colnames(mat1) = paste0("column_1_", seq_len(nrow(mat1)))

nr3 = 10; nr1 = 8; nr2 = 6
nc3 = 6; nc1 = 8; nc2 = 10
mat2 = cbind(rbind(matrix(rnorm(nr1*nc1, mean = 1,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc1, mean = 0,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc1, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc2, mean = 0,   sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc2, mean = 1,   sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc2, mean = 0,   sd = 0.5), nr = nr3)),
    rbind(matrix(rnorm(nr1*nc3, mean = 0.5, sd = 0.5), nr = nr1),
          matrix(rnorm(nr2*nc3, mean = 0.5, sd = 0.5), nr = nr2),
          matrix(rnorm(nr3*nc3, mean = 1,   sd = 0.5), nr = nr3))
   )

rownames(mat2) = paste0("row_2_", seq_len(nrow(mat2)))
colnames(mat2) = paste0("column_2_", seq_len(nrow(mat2)))


ht_list = Heatmap(mat1) + Heatmap(mat2)
ht_list

######### legend ############
draw(ht_list, heatmap_legend_side = "bottom")
draw(ht_list, heatmap_legend_side = "left")
draw(ht_list, heatmap_legend_side = "top")


########## width #############
Heatmap(mat1, width = unit(6, "cm")) + Heatmap(mat2)
Heatmap(mat1) + Heatmap(mat2, width = unit(8, "cm"))
Heatmap(mat1, width = unit(12, "cm")) + Heatmap(mat2, width = unit(8, "cm"))

Heatmap(mat1, heatmap_body_width = unit(6, "cm")) + Heatmap(mat2)
Heatmap(mat1) + Heatmap(mat2, heatmap_body_width = unit(6, "cm"))
Heatmap(mat1, heatmap_body_width = unit(6, "cm")) + Heatmap(mat2, heatmap_body_width = unit(6, "cm"))
Heatmap(mat1, heatmap_body_width = 4) + Heatmap(mat2)

Heatmap(mat1, heatmap_body_width = 2) + Heatmap(mat2, heatmap_body_width = 1)


########### height ###########
Heatmap(mat1, height = unit(6, "cm")) + Heatmap(mat2)
Heatmap(mat1, heatmap_body_height = unit(6, "cm")) + Heatmap(mat2)
ht_list = Heatmap(mat1, heatmap_body_width = unit(6, "cm"), height = unit(6, "cm")) + 
	Heatmap(mat2, heatmap_body_width = unit(6, "cm"), height = unit(6, "cm"))
draw(ht_list, column_title = "foooooooooo", row_title = "baaaaaaaaaaar")

##### split #####
ht_list = Heatmap(mat1, name = "m1", row_km = 2) + Heatmap(mat2, name = "m2", row_km = 3)
draw(ht_list, main_heatmap = "m1")
draw(ht_list, main_heatmap = "m2")

ht_list = Heatmap(mat1, name = "m1", row_km = 2, column_km = 3, width = unit(8, "cm"), height = unit(6, "cm")) + 
	Heatmap(mat2, name = "m2", row_km = 3, column_km = 2, width = unit(8, "cm"), height = unit(10, "cm"))
draw(ht_list, main_heatmap = "m1", column_title = "foooooooooo", row_title = "baaaaaaaaaaar")
draw(ht_list, main_heatmap = "m2", column_title = "foooooooooo", row_title = "baaaaaaaaaaar")

##### adjust column annotations #####
ha1 = HeatmapAnnotation(foo = 1:24, bar = anno_points(24:1, height = unit(4, "cm")))
ha2 = HeatmapAnnotation(bar = anno_points(24:1), foo = 1:24)
Heatmap(mat1, top_annotation = ha1) + Heatmap(mat2, top_annotation = ha2)
ha2 = HeatmapAnnotation(foo = 1:24)
Heatmap(mat1, top_annotation = ha1) + Heatmap(mat2, top_annotation = ha2)
Heatmap(mat1, top_annotation = ha1) + Heatmap(mat2)
Heatmap(mat1, bottom_annotation = ha1) + Heatmap(mat2)


#### row annotations #####
ha = rowAnnotation(foo = 1:24, bar = anno_points(24:1), width = unit(6, "cm"))
Heatmap(mat1) + ha
Heatmap(mat1, width = unit(6, "cm")) + ha
Heatmap(mat1, width = unit(6, "cm"), row_km = 2) + ha



