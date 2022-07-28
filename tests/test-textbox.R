
library(ComplexHeatmap)

words = sapply(1:30, function(x) strrep(sample(letters, 1), sample(3:10, 1)))
grid.newpage()
grid.textbox(words, gp = gpar(fontsize = runif(30, min = 5, max = 30)))

sentenses = c("This is sentense 1", "This is a long long long long long long long sentense.")
grid.newpage()
grid.textbox(sentenses)
grid.textbox(sentenses, word_wrap = TRUE)
grid.textbox(sentenses, word_wrap = TRUE, add_new_line = TRUE)


require(circlize)
mat = matrix(rnorm(100*10), nrow = 100)

split = sample(letters[1:10], 100, replace = TRUE)
text = lapply(unique(split), function(x) {
	data.frame(month.name, col = rand_color(12, friendly = TRUE), fontsize = runif(12, 6, 14))
})
names(text) = unique(split)

Heatmap(mat, cluster_rows = FALSE, row_split = split,
    right_annotation = rowAnnotation(wc = anno_textbox(split, text))
)
