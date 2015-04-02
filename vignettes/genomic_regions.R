library(ComplexHeatmap)
library(circlize)

rand_meth = function(k, mean) {
	(runif(k) - 0.5)*min(c(1-mean), mean) + mean
}

mean_meth = c(rand_meth(300, 0.3), rand_meth(700, 0.7))
mat_meth = as.data.frame(lapply(mean_meth, function(m) {
	if(m < 0.3) {
		c(rand_meth(10, m), rand_meth(10, m + 0.2))
	} else if(m > 0.7) {
		c(rand_meth(10, m), rand_meth(10, m - 0.2))
	} else {
		c(rand_meth(10, m), rand_meth(10, m + sample(c(1, -1), 1)*0.2))
	}

}))
mat_meth = t(mat_meth)
rownames(mat_meth) = NULL
colnames(mat_meth) = paste0("sample", 1:20)

direction = rowMeans(mat_meth[, 1:10]) - rowMeans(mat_meth[, 11:20])
direction = ifelse(direction > 0, "hyper", "hypo")

mat_expr = t(apply(mat_meth, 1, function(x) {
	x = x + rnorm(length(x), sd = (runif(1)-0.5)*0.4 + 0.5)
	scale(x)
}))
dimnames(mat_expr) = dimnames(mat_meth)

cor_pvalue = -log10(sapply(seq_len(nrow(mat_meth)), function(i) {
	cor.test(mat_meth[i, ], mat_expr[i, ])$p.value
}))

gene_type = sample(c("protein_coding", "lincRNA", "microRNA", "psedo-gene", "others"), nrow(mat_meth), replace = TRUE, prob = c(6, 1, 1, 1, 1))

anno_gene = sapply(mean_meth, function(m) {
	if(m > 0.6) {
		if(runif(1) < 0.8) return("intragenic")
	}
	if(m < 0.3) {
		if(runif(1) < 0.4) return("TSS")
	}
	return("intergenic")
})

dist = sapply(mean_meth, function(m) {
	if(m < 0.6) {
		if(runif(1) < 0.8) return(round( (runif(1)-0.5)*1000000 + 500000 ))
	}
	if(m < 0.3) {
		if(runif(1) < 0.4) return(round( (runif(1) - 0.5)*1000 + 500))
	}
	return(round( (runif(1) - 0.5)*100000 + 50000))
})


anno_enhancer_1 = sapply(mean_meth, function(m) {
	if(m < 0.3) {
		if(runif(1) < 0.6) return(runif(1))
	} else if (runif(1) < 0.1) {
		return(runif(1))
	} 
	return(0)
})


anno_enhancer_2 = sapply(mean_meth, function(m) {
	if(m < 0.3) {
		if(runif(1) < 0.6) return(runif(1))
	} else if (runif(1) < 0.1) {
		return(runif(1))
	} 
	return(0)
})

anno_enhancer_3 = sapply(mean_meth, function(m) {
	if(m < 0.3) {
		if(runif(1) < 0.6) return(runif(1))
	} else if (runif(1) < 0.1) {
		return(runif(1))
	} 
	return(0)
})

anno_enhancer = data.frame(enhancer_1 = anno_enhancer_1, enhancer_2 = anno_enhancer_2, enhancer_3 = anno_enhancer_3)

ha = HeatmapAnnotation(df = data.frame(type = c(rep("Tumor", 10), rep("Control", 10))))
ha2 = HeatmapAnnotation(df = data.frame(type = c(rep("Tumor", 10), rep("Control", 10))), show_legend = FALSE)

column_tree = hclust(dist(t(mat_meth)))

ht_list = Heatmap(mat_meth, name = "methylation", col = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")),
	cluster_columns = column_tree, top_annotation = ha, column_names_gp = gpar(fontsize = 8), km = 5, column_title = "Methylation", column_title_gp = gpar(fontsize = 10), row_title_gp = gpar(fontsize = 10)) +
	Heatmap(direction, name = "direction", col = c("hyper" = "red", "hypo" = "blue"), column_names_gp = gpar(fontsize = 8)) +
	Heatmap(mat_expr[, column_tree$order], name = "expression", col = colorRamp2(c(-2, 0, 2), c("green", "white", "red")),
		cluster_columns = FALSE, top_annotation = ha2, column_names_gp = gpar(fontsize = 8), column_title = "Expression", column_title_gp = gpar(fontsize = 10)) +
	Heatmap(cor_pvalue, name = "-log10(cor_p)", col = colorRamp2(c(0, 2, 4), c("white", "white", "red")), column_names_gp = gpar(fontsize = 8)) +
	Heatmap(gene_type, name = "gene type", column_names_gp = gpar(fontsize = 8)) +
	Heatmap(anno_gene, name = "anno_gene", column_names_gp = gpar(fontsize = 8)) +
	Heatmap(dist, name = "dist_tss", col = colorRamp2(c(0, 10000), c("black", "white")), column_names_gp = gpar(fontsize = 8)) +
	Heatmap(anno_enhancer, name = "anno_enhancer", col = colorRamp2(c(0, 1), c("white", "orange")), cluster_columns = FALSE, column_names_gp = gpar(fontsize = 8), column_title = "Enhancer", column_title_gp = gpar(fontsize = 10))

#pdf("genomic_regions.pdf", width = 10, height = 10)
draw(ht_list, newpage = FALSE, column_title = "Comprehensive correspondence between methylation, expression and other genomic features", column_title_gp = gpar(fontsize = 12, fontface = "bold"), heatmap_legend_side = "bottom", legend_title_gp = gpar(fontsize = 8, fontface = "bold"), legend_label_gp = gpar(fontsize = 8))
#dev.off()

