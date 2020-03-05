

suppressWarnings(suppressPackageStartupMessages(library(ComplexHeatmap)))

## test the scripts in test-plot folder
for(f in dir("test-plot")) {
	pdf(NULL)
	source(f)
	dev.off()
}

library(testthat)

test_check("ComplexHeatmap")
