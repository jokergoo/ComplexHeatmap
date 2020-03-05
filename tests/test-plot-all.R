suppressWarnings(suppressPackageStartupMessages(library(ComplexHeatmap)))
library(circlize)
library(GetoptLong)

## test the scripts in test-plot folder
for(f in list.files("test-plot", full.names = TRUE)) {
	pdf(NULL)
	source(f)
	dev.off()
}
