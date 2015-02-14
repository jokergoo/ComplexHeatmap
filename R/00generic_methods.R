
set_generic_functions = function(fname) {

	op = qq.options(READ.ONLY = FALSE)
    on.exit(qq.options(op))
    qq.options(code.pattern = "@\\{CODE\\}")

	for(f in unique(fname)) {
		
		eval(parse(text = qq("
if(!isGeneric('@{f}')) {
    if(is.function('@{f}')) {
        fun = @{f}
    } else {
        fun = function(object, ...) standardGeneric('@{f}', ...)
    }
    setGeneric('@{f}', fun)
}
")))
	}
}

set_generic_functions(c(
	"map", 
	"legend",

	"add_heatmap",
	"draw_heatmap_body",
	"draw_hclust",
	"draw_dimnames",
	"draw_title",
	"draw_annotation",
	"component_width",
	"component_height",
	"set_component_height",
	"draw",

	"add_heatmap",
	"make_layout",
	"draw",
	"component_width",
	"component_height",
	"draw_heatmap_list",
	"draw_title",
	"draw_heatmap_legend",
	"draw_annotation_legend",
	"heatmap_legend_size",
	"annotation_legend_size"
))
