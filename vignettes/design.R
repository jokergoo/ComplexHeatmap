

par(mfrow = c(1, 2))

par(mar = c(1, 1, 1, 1))
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)

text = c("annotation legend", "heatmap legend", "title", "title", "heatmap legend", "annotation legend")
offset = c(0, 0.1, 0.2, 0.7, 0.8, 0.9)
for(i in seq_along(offset)) {
	rect(0 + offset[i], 0.3, 0.1 + offset[i], 0.7)
	text(0.05 + offset[i], 0.5, text[i], srt = 90)
}

text = c("annotation legend", "heatmap legend", "title", "title", "heatmap legend", "annotation legend")
offset = c(0, 0.1, 0.2, 0.7, 0.8, 0.9)
for(i in seq_along(offset)) {
	rect(0.3, 0 + offset[i], 0.7, 0.1 + offset[i])
	text(0.5, 0.05 + offset[i], text[i])
}

rect(0.31, 0.31, 0.31+0.4/3, 0.69)
text(0.3+0.2/3, (0.31+0.69)/2, "heatmap 1", srt = 90)
rect(0.31+0.4/3, 0.31, 0.31+0.8/3, 0.69)
text(0.5, (0.31+0.69)/2, "heatmap 2", srt = 90)
rect(0.31+0.8/3, 0.31, 0.69, 0.69)
text(0.31+1/3, (0.31+0.69)/2, "row annotation", srt = 90)

mtext("heatmap list", side = 3, font = 2)


par(mar = c(1, 1, 1, 1))
plot(NULL, xlim = c(0, 1), ylim = c(0, 1), ann = FALSE, axes = FALSE)

rect(0, 0.2, 0.3, 0.8)
text = c("title", "dendrogram", "row names", "row names", "dendrogram", "title")
offset = c(0, 0.1, 0.2, 0.7, 0.8, 0.9)
for(i in seq_along(offset)) {
	rect(0 + offset[i] + 0.005, 0.5+0.01, 0.1 + offset[i]-0.005, 0.8-0.01)
	text(0.05 + offset[i], 0.65, text[i], srt = 90)
}
rect(0.7, 0.2, 1, 0.8)
for(i in seq_along(offset)) {
	rect(0 + offset[i]+0.005, 0.2+0.01, 0.1 + offset[i]-0.005, 0.5-0.01)
	text(0.05 + offset[i], 0.35, text[i], srt = 90)
}

text = c("title", "dendrogram", "annotation", "column names", "column names", "annotation", "dendrogram", "title")
offset = c(0, 0.05, 0.1, 0.15, 0.8, 0.85, 0.9, 0.95)
for(i in seq_along(offset)) {
	rect(0.3, 0 + offset[i], 0.7, 0.05 + offset[i])
	text(0.5, 0.025 + offset[i], text[i])
}

rect(0.31, 0.505, 0.69, 0.79)
text(0.5, (0.505+0.79)/2, "matrix\nrow slice 1")

rect(0.31, 0.495, 0.69, 0.21)
text(0.5, (0.505+0.31)/2, "matrix\nrow slice 2")

mtext("single heatmap", side = 3, font = 2)

par(xpd = NA)

# arrows(-0.75, 0.65, -0.05, 0.65, angle = 15)
# arrows(-0.55, 0.35, -0.05, 0.35, angle = 15)

