Make Complex Heatmap
=========================

The aim of this package is to place any numbers of heatmaps, and any types of annotation graphics (both on rows and columns).
Then it would be very helpful to visualize multiple sources of data and reveal potential correlations.

A single heatmap or a list of heatmaps are composed by basic components:

![default](https://cloud.githubusercontent.com/assets/449218/6541828/75c77f8a-c4e5-11e4-80af-6ebb5e649898.png)

The package makes heatmaps in an object oriented way by abstracting heatmaps into several classes:

- `Heatmap` a single heatmap
- `HeatmapList` a list of heatmaps
- `HeatmapAnnotation` annotation on columns or rows

and provides methods for arranging and plotting each heatmap components.

## Install

`ComplexHeatmap` is available on Bioconductor, you can intall it by:

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
```

## Usage

Make a single heatmap:

```{r}
Heatmap(mat, ...)
```

A single Heatmap with column annotations:

```{r}
ha = HeatmapAnnotation(anno1, anno2, ...)
Heatmap(mat, ..., top_annotation = ha)
```

Make a list of heatmaps:

```{r}
Heatmap(mat1, ...) + Heatmap(mat2, ...)
```

Make a list of heatmaps and row annotations:

```{r}
ha = HeatmapAnnotation(anno1, anno2, ..., which = "row")
Heatmap(mat1, ...) + Heatmap(mat2, ...) + ha
```

## Examples


The first examples is <a href="http://www.cbioportal.org/faq.jsp#what-are-oncoprints">OncoPrint</a>.
The basic idea is to self define the heatmap body. Besides the default style which is 
provided by <a href="http://www.cbioportal.org/index.do">cBioPortal</a>, there are
additional barplots at both sides of the heatmap which show numbers of different alterations for
each sample and for each gene.

The second examples visualizes correlation between methylation and expression, as well as other annotation information (data are randomly generated). In the heatmap, each row corresponds to a differentially methylated regions (DMRs). 
From left to right, heatmaps are:

1. methylation for each DMRs in samples.
2. direction of the methylation (one column heatmap), i.e. is methylation hyper in tumor or hypo?
3. expression for the genes that are associated with corresponding DMRs (e.g. closest gene).
4. signiciance for the correlation between methylation and expression (-log10(p-value)).
5. type of genes, i.e. is the gene a protein coding gene or a lincRNA?
6. annotation to gene models, i.e. is the DMR located in the intragenic region of the corresponding gene or the DMR is intergenic?
7. distance from the DMR to the TSS of the corresponding gene.
8. overlapping between DMRs and enhancers (Color shows how much the DMR is covered by the enhancers).

![example](https://cloud.githubusercontent.com/assets/449218/6862097/1bc46436-d443-11e4-91f5-431bc9210c80.png)
