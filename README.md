[![Build Status](https://travis-ci.org/jokergoo/ComplexHeatmap.svg)](https://travis-ci.org/jokergoo/ComplexHeatmap)

Make Complex Heatmaps
=========================

Complex heatmaps are efficient to visualize associations between different sources of data sets and reveal potential features. Here the ComplexHeatmap package provides a highly flexible way to arrange multiple heatmaps and supports self-defined annotation graphics.

A single heatmap or a list of heatmaps are composed by basic components:

![default](https://cloud.githubusercontent.com/assets/449218/6541828/75c77f8a-c4e5-11e4-80af-6ebb5e649898.png)

The package makes heatmaps in an object-oriented way by abstracting heatmaps into several classes:

- `Heatmap` a single heatmap
- `HeatmapList` a list of heatmaps
- `HeatmapAnnotation` annotation on columns or rows

and provides methods for arranging and plotting heatmap components.

## Install

`ComplexHeatmap` is available on Bioconductor, you can intall it by:

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
```

If you want the latest version, install it directly from GitHub:

```{r}
library(devtools)
install_github("jokergoo/ComplexHeatmap")
```

## Usage

Make a single heatmap:

```{r}
Heatmap(mat, ...)
```

A single Heatmap with column annotations:

```{r}
ha = HeatmapAnnotation(df = anno1, anno_fun = anno2, ...)
Heatmap(mat, ..., top_annotation = ha)
```

Make a list of heatmaps:

```{r}
Heatmap(mat1, ...) + Heatmap(mat2, ...)
```

Make a list of heatmaps and row annotations:

```{r}
ha = HeatmapAnnotation(df = anno1, anno_fun = anno2, ..., which = "row")
Heatmap(mat1, ...) + Heatmap(mat2, ...) + ha
```

## As a base package

**ComplexHeatmap** can be used as a base package to build other packages which focus
On specific applications. E.g. [EnrichedHeatmap](http://github.com/jokergoo/EnrichedHeatmap) package
uses **ComplexHeatmap** as base to make heatmaps which visualize the enrichment of genomic signals
to specific target regions.

## Visualize high dimensional genomic data

The examples visualizes correlation between methylation and expression, as well as other annotation information (data are randomly generated). In the heatmap, each row corresponds to a differentially methylated regions (DMRs). 
From left to right, heatmaps are:

1. methylation for each DMRs in samples.
2. direction of the methylation (one column heatmap), i.e. is methylation hyper in tumor or hypo?
3. expression for the genes that are associated with corresponding DMRs (e.g. closest gene).
4. signiciance for the correlation between methylation and expression (-log10(p-value)).
5. type of genes, i.e. is the gene a protein coding gene or a lincRNA?
6. annotation to gene models, i.e. is the DMR located in the intragenic region of the corresponding gene or the DMR is intergenic?
7. distance from the DMR to the TSS of the corresponding gene.
8. overlapping between DMRs and enhancers (Color shows how much the DMR is covered by the enhancers).

![download](https://cloud.githubusercontent.com/assets/449218/9685180/dddf30c0-531c-11e5-805a-4cc5a36e9197.png)

## OncoPrint

<a href="http://www.cbioportal.org/faq.jsp#what-are-oncoprints">OncoPrint</a> visualize multiple genomic alteration
events through a heatmap. From verion 1.3.0, **ComplexHeatmap** package provides a new `oncoPrint()` function. By this
function, users can define their own graphics which correspond to differnet alteration events. Also the function additionally
add barplots on two sides of the heatmap which tell number of different alterations in patients or samples.

With general functionality of **ComplexHeamtap**, you can add more heatmaps / row annotations to the oncoPrint, even split the
oncoPrint to enphasize sub groups.

![oncoprint](https://cloud.githubusercontent.com/assets/449218/9370313/9c9b6b00-46cf-11e5-9740-c5c2a7a40eb5.png)

## Interact with heatmaps

You can use mouse to select a region on the heatmap, it will return row index and column index which correspond to the selected region.

![download](https://cloud.githubusercontent.com/assets/449218/9685087/456d6276-531c-11e5-9837-2ba8a081ad50.gif)
