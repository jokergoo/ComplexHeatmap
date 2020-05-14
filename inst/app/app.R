require(shiny)

ui = fluidPage(
	fluidRow(
		column(width = 6,
			plotOutput("plot1", height = 300, width = 300,
		        brush = brushOpts(id = "plot1_brush")
		     )
		),
		column(width = 6,
		  plotOutput("brush_info", height = 300, width = 300)
		)
	)
)

server = function(input, output) {
	output$plot1 = renderPlot({
		draw(ht_list)
	})

	output$brush_info = renderPlot({
		if(is.null(input$plot1_brush)) {
			
		} else {
		  	coords = input$plot1_brush$coords_css
		    rg = input$plot1_brush$range
		    height = (rg$bottom - rg$top)/input$plot1_brush$img_css_ratio$y
		    pos1 = unit(c(coords$xmin, height - coords$ymin), "pt")
		    pos2 = unit(c(coords$xmax, height - coords$ymax), "pt")

		    # png("foo.png", width = rg$right - rg$left, height = rg$bottom - rg$top)
		    # this makes a plot, but it will be overwritten by the next one
		    ht_list = draw(ht_list)
		    pos = selectArea(ht_list, pos1 = pos1, pos2 = pos2, verbose = FALSE)
		    # dev.off()
		    # file.remove("foo.png")
		    row_index = pos[[1]][[1]]$row_index
		    column_index = pos[[1]][[1]]$column_index
		    ht_select = Heatmap(m[row_index, column_index, drop = FALSE],
		    	col = ht_list@ht_list[[1]]@matrix_color_mapping@col_fun,
		    	show_heatmap_legend = FALSE,
		    	cluster_rows = FALSE, cluster_columns = FALSE)
		    draw(ht_select)
		}
	})
}
