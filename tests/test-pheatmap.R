library(ComplexHeatmap)

if(requireNamespace("pheatmap")) {
	mat = matrix(rnorm(100), 10)

	compare_pheatmap(mat)

	pheatmap(mat)
	pheatmap(mat, col = rev(RColorBrewer::brewer.pal(n = 7, name = "RdYlBu")))

	test = matrix(rnorm(200), 20, 10)
	test[1:10, seq(1, 10, 2)] = test[1:10, seq(1, 10, 2)] + 3
	test[11:20, seq(2, 10, 2)] = test[11:20, seq(2, 10, 2)] + 2
	test[15:20, seq(2, 10, 2)] = test[15:20, seq(2, 10, 2)] + 4
	colnames(test) = paste("Test", 1:10, sep = "")
	rownames(test) = paste("Gene", 1:20, sep = "")

	# Draw heatmaps
	compare_pheatmap(test)
	compare_pheatmap(test, kmeans_k = 2)
	compare_pheatmap(test, scale = "row", clustering_distance_rows = "correlation")
	compare_pheatmap(test, color = colorRampPalette(c("navy", "white", "firebrick3"))(50))
	compare_pheatmap(test, cluster_row = FALSE)
	compare_pheatmap(test, legend = FALSE)

	# Show text within cells
	compare_pheatmap(test, display_numbers = TRUE)
	compare_pheatmap(test, display_numbers = TRUE, number_format = "%.1e")
	compare_pheatmap(test, display_numbers = matrix(ifelse(test > 5, "*", ""), nrow(test)))
	compare_pheatmap(test, cluster_row = FALSE, legend_breaks = -1:4, legend_labels = c("0",
		"1e-4", "1e-3", "1e-2", "1e-1", "1"))

	# Fix cell sizes and save to file with correct size
	compare_pheatmap(test, cellwidth = 15, cellheight = 12, main = "Example heatmap")

	# Generate annotations for rows and columns
	annotation_col = data.frame(
	    CellType = factor(rep(c("CT1", "CT2"), 5)), 
	    Time = 1:5
	)
	rownames(annotation_col) = paste("Test", 1:10, sep = "")

	annotation_row = data.frame(
	    GeneClass = factor(rep(c("Path1", "Path2", "Path3"), c(10, 4, 6)))
	)
	rownames(annotation_row) = paste("Gene", 1:20, sep = "")

	# Display row and color annotations
	compare_pheatmap(test, annotation_col = annotation_col)
	compare_pheatmap(test, annotation_col = annotation_col, annotation_legend = FALSE)
	compare_pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row)

	# Change angle of text in the columns
	compare_pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row, angle_col = "45")
	compare_pheatmap(test, annotation_col = annotation_col, angle_col = "0")

	# Specify colors
	ann_colors = list(
	    Time = c("white", "firebrick"),
	    CellType = c(CT1 = "#1B9E77", CT2 = "#D95F02"),
	    GeneClass = c(Path1 = "#7570B3", Path2 = "#E7298A", Path3 = "#66A61E")
	)

	compare_pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors, main = "Title")
	compare_pheatmap(test, annotation_col = annotation_col, annotation_row = annotation_row, 
	         annotation_colors = ann_colors)
	compare_pheatmap(test, annotation_col = annotation_col, annotation_colors = ann_colors[2]) 

	# Gaps in heatmaps
	compare_pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14))
	compare_pheatmap(test, annotation_col = annotation_col, cluster_rows = FALSE, gaps_row = c(10, 14), 
	         cutree_col = 2)

	# Show custom strings as row/col names
	labels_row = c("", "", "", "", "", "", "", "", "", "", "", "", "", "", "", 
		"", "", "Il10", "Il15", "Il1b")

	compare_pheatmap(test, annotation_col = annotation_col, labels_row = labels_row)

	# Specifying clustering from distance matrix
	drows = dist(test, method = "minkowski")
	dcols = dist(t(test), method = "minkowski")
	compare_pheatmap(test, clustering_distance_rows = drows, clustering_distance_cols = dcols)

	library(dendsort)

	callback = function(hc, ...){dendsort(hc)}
	compare_pheatmap(test, clustering_callback = callback)
}


set.seed(42)
nsamples <- 10

mat <- matrix(rpois(20*nsamples, 20), ncol=nsamples)
colnames(mat) <- paste0("sample", seq_len(ncol(mat)))
rownames(mat) <- paste0("gene", seq_len(nrow(mat)))

annot <- data.frame(
  labs = sample(c("A","B","C","D"), size = ncol(mat), replace = TRUE),
  row.names = colnames(mat)
)
ins <- list(mat = mat, annotation_col = annot)
do.call(ComplexHeatmap::pheatmap, ins[1])
do.call(ComplexHeatmap::pheatmap, ins)
