# Make Complex Heatmaps <a href="https://jokergoo.github.io/ComplexHeatmap-reference/book/"><img src="https://jokergoo.github.io/ComplexHeatmap-reference/book/complexheatmap-cover.jpg" width=240 align="right" style="border:2px solid black;" ></a>

[![Build Status](https://travis-ci.org/jokergoo/ComplexHeatmap.svg)](https://travis-ci.org/jokergoo/ComplexHeatmap) 
[![codecov](https://img.shields.io/codecov/c/github/jokergoo/ComplexHeatmap.svg)](https://codecov.io/github/jokergoo/ComplexHeatmap) 
[![bioc](http://www.bioconductor.org/shields/downloads/devel/ComplexHeatmap.svg)](https://bioconductor.org/packages/stats/bioc/ComplexHeatmap/) 
[![bioc](http://www.bioconductor.org/shields/years-in-bioc/ComplexHeatmap.svg)](http://bioconductor.org/packages/devel/bioc/html/ComplexHeatmap.html)

<img src="http://jokergoo.github.io/complexheatmap_logo.svg" width="550">


Complex heatmaps are efficient to visualize associations between different
sources of data sets and reveal potential patterns. Here the
**ComplexHeatmap** package provides a highly flexible way to arrange multiple
heatmaps and supports various annotation graphics.

## Citation

Zuguang Gu, Roland Eils and Matthias Schlesner, [Complex heatmaps reveal patterns and correlations in multidimensional genomic data](http://bioinformatics.oxfordjournals.org/content/early/2016/05/20/bioinformatics.btw313.abstract), Bioinformatics, 2016


## Install

`ComplexHeatmap` is available on [Bioconductor](http://www.bioconductor.org/packages/devel/bioc/html/ComplexHeatmap.html), you can install it by:

```r
if (!requireNamespace("BiocManager", quietly=TRUE))
    install.packages("BiocManager")
BiocManager::install("ComplexHeatmap")
```

If you want the latest version, install it directly from GitHub:

```r
library(devtools)
install_github("jokergoo/ComplexHeatmap")
```

## Usage

Make a single heatmap:

```r
Heatmap(mat, ...)
```

A single Heatmap with column annotations:

```r
ha = HeatmapAnnotation(df = anno1, anno_fun = anno2, ...)
Heatmap(mat, ..., top_annotation = ha)
```

Make a list of heatmaps:

```r
Heatmap(mat1, ...) + Heatmap(mat2, ...)
```

Make a list of heatmaps and row annotations:

```r
ha = HeatmapAnnotation(df = anno1, anno_fun = anno2, ..., which = "row")
Heatmap(mat1, ...) + Heatmap(mat2, ...) + ha
```

## Documentation

The full documentations are available at https://jokergoo.github.io/ComplexHeatmap-reference/book/ and the website is at https://jokergoo.github.io/ComplexHeatmap.

## Blog posts

There are following blog posts focusing on specific topics:

- [Translate from pheatmap to ComplexHeatmap](https://jokergoo.github.io/2020/05/06/translate-from-pheatmap-to-complexheatmap/)
- [Set cell width/height in the heatmap](https://jokergoo.github.io/2020/05/11/set-cell-width/height-in-the-heatmap/)
- [Interactive ComplexHeatmap](https://jokergoo.github.io/2020/05/15/interactive-complexheatmap/)
- [Word cloud as heatmap annotation](https://jokergoo.github.io/2020/05/31/word-cloud-as-heatmap-annotation/)
- [Which heatmap function is faster?](https://jokergoo.github.io/2020/06/19/which-heatmap-function-is-faster/)
- [Rasterization in ComplexHeatmap](https://jokergoo.github.io/2020/06/30/rasterization-in-complexheatmap/)
- [Block annotation over several slices](https://jokergoo.github.io/2020/07/06/block-annotation-over-several-slices/)
- [Integrate ComplexHeatmap with cowplot package](https://jokergoo.github.io/2020/07/14/integrate-complexheatmap-with-cowplot-package/)


## Examples

### Visualize Methylation Profile with Complex Annotations

![complexheatmap_example4](https://user-images.githubusercontent.com/449218/47718635-2ec22980-dc49-11e8-9f01-37becb19e0d5.png)

### Correlations between methylation, expression and other genomic features

![complexheatmap_example3](https://user-images.githubusercontent.com/449218/47718636-2ec22980-dc49-11e8-8db0-1659c27dcf40.png)

### Visualize Cell Heterogeneity from Single Cell RNASeq

![complexheatmap_example2](https://user-images.githubusercontent.com/449218/47718637-2ec22980-dc49-11e8-925e-955c16cfa982.png)

### Making Enhanced OncoPrint

![complexheatmap_example1](https://user-images.githubusercontent.com/449218/47718638-2ec22980-dc49-11e8-845e-21e51d3b8e73.png)

### UpSet plot

<img src="https://user-images.githubusercontent.com/449218/102615477-48c76a80-4136-11eb-98d9-3c528844fbe8.png" width=500 />

## License

MIT @ Zuguang Gu

