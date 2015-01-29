

cm = colorMapping(name = "test",
	colors = c("blue", "white", "red"),
	levels = c("a", "b", "c"))
cm$map("a")
cm$map("d")
cm$map(c("a", "a", "b", "c"))


cm = colorMapping(name = "test",
	col_fun = colorRamp2(c(0, 0.5, 1), c("blue", "white", "red")))


cm$map(0)
cm$map(2)
cm$map(seq(-1, 2, length = 10))


pushViewport()
cm$legend
