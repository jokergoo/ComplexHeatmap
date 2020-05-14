require(shiny)
library(GetoptLong)

ui = fluidPage(
	h3("Click on the heatmap or select a region from the heatmap."),
	fluidRow(
		column(width = 3,
			plotOutput("plot1", height = 300, width = 300,
		        brush = brushOpts(id = "plot1_brush"),
		        click = "plot1_click"
		    ),
			verbatimTextOutput("click_info")
		),
		column(width = 3,
		  plotOutput("brush_info", height = 300, width = 300)
		)
	)
)

shiny_env = new.env()

server = function(input, output) {
	output$plot1 = renderPlot({
		shiny_env$ht_list = draw(ht_list)
		shiny_env$ht_pos = ht_pos_on_device(shiny_env$ht_list)
	})

	output$brush_info = renderPlot({
		if(is.null(input$plot1_brush)) {
			grid.newpage()
			grid.text("No region is selected.", 0.5, 0.5)
		} else {
		  	lt = ComplexHeatmap:::get_pos_from_brush(input$plot1_brush)
		  	pos1 = lt[[1]]
		  	pos2 = lt[[2]]
		    
		    ht_list = shiny_env$ht_list
		    pos = selectArea(ht_list, mark = FALSE, pos1 = pos1, pos2 = pos2, verbose = FALSE, ht_pos = shiny_env$ht_pos)
		    
		    row_index = pos[[1]][[1]]$row_index
		    column_index = pos[[1]][[1]]$column_index
		    m = ht_list@ht_list[[1]]@matrix
		    ht_select = Heatmap(m[row_index, column_index, drop = FALSE],
		    	col = ht_list@ht_list[[1]]@matrix_color_mapping@col_fun,
		    	show_heatmap_legend = FALSE,
		    	cluster_rows = FALSE, cluster_columns = FALSE)
		    draw(ht_select)
		}
	})

	output$click_info = renderText({
		if(is.null(input$plot1_click)) {
			"Not selected."
		} else {
			pos1 = ComplexHeatmap:::get_pos_from_click(input$plot1_click)
		    
		    ht_list = shiny_env$ht_list
		    pos = selectPosition(ht_list, mark = FALSE, pos = pos1, verbose = FALSE, ht_pos = shiny_env$ht_pos)
			
			row_index = pos[[1]][[1]]$row_index
		    column_index = pos[[1]][[1]]$column_index
		    m = ht_list@ht_list[[1]]@matrix
		    v = m[row_index, column_index]
			qq("row index: @{row_index}\ncolumn index: @{column_index}\nvalue: @{v}")
		}
	})
}
