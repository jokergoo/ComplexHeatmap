

cm = ColorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c("a", "b", "c"))
color_mapping_legend(cm)

cm = ColorMapping(name = "test",
	col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")))
color_mapping_legend(cm)

cm = ColorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c(1, 2, 3))
color_mapping_legend(cm)
