[![Build Status](https://travis-ci.org/jokergoo/ComplexHeatmap.svg)](https://travis-ci.org/jokergoo/ComplexHeatmap) 
[![codecov](https://img.shields.io/codecov/c/github/jokergoo/ComplexHeatmap.svg)](https://codecov.io/github/jokergoo/ComplexHeatmap) 
[![bioc](http://www.bioconductor.org/shields/downloads/ComplexHeatmap.svg)](https://bioconductor.org/packages/stats/bioc/ComplexHeatmap/) 
[![bioc](http://mcube.nju.edu.cn/cgi-bin/zuguanggu/bioc_download.pl?package=ComplexHeatmap&)](https://bioconductor.org/packages/stats/bioc/ComplexHeatmap/) 
[![bioc](http://www.bioconductor.org/shields/years-in-bioc/ComplexHeatmap.svg)](http://bioconductor.org/packages/devel/bioc/html/ComplexHeatmap.html)

## Make Complex Heatmaps

Complex heatmaps are efficient to visualize associations 
between different sources of data sets and reveal potential structures. 
Here the **ComplexHeatmap** package provides a highly flexible way to arrange 
multiple heatmaps and supports self-defined annotation graphics.

### Citation

Zuguang Gu, Roland Eils and Matthias Schlesner, [Complex heatmaps reveal patterns and correlations in multidimensional genomic data](http://bioinformatics.oxfordjournals.org/content/early/2016/05/20/bioinformatics.btw313.abstract), Bioinformatics, 2016

### General design

Generally, a heatmap list contains several heatmaps and row annotations.

![default](https://cloud.githubusercontent.com/assets/449218/6541828/75c77f8a-c4e5-11e4-80af-6ebb5e649898.png)

Surrounding the heatmap list,
there are legends for heatmaps and annotations, also there are titles which are placed
on the four sides of the heatmap list. And for each heatmap, there are also different components
surrounding the heatmap body.

The **ComplexHeatmap** package is implemented in an object-oriented way. To describe a heatmap list, 
there are following classes:

- `Heatmap` class: a single heatmap containing heatmap body, row/column names, titles, dendrograms and column annotations.
- `HeatmapList` class: a list of heatmaps and row annotations.
- `HeatmapAnnotation` class: defines a list of row annotations and column annotations.

There are also several internal classes:

- `SingleAnnotation` class: defines a single row annotation or column annotation.
- `ColorMapping` class: mapping from values to colors.

**ComplexHeatmap** is implemented under **grid** system, so users should know basic **grid** functionality
to get full use of the package.

### Install

`ComplexHeatmap` is available on [Bioconductor](http://www.bioconductor.org/packages/devel/bioc/html/ComplexHeatmap.html), you can intall it by:

```{r}
source("http://bioconductor.org/biocLite.R")
biocLite("ComplexHeatmap")
```

If you want the latest version, install it directly from GitHub:

```{r}
library(devtools)
install_github("jokergoo/ComplexHeatmap")
```

### Usage

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

### As a base package

**ComplexHeatmap** can be used as a base package to build other packages which focus
On specific applications. E.g. [EnrichedHeatmap](http://github.com/jokergoo/EnrichedHeatmap) package
uses **ComplexHeatmap** as base to make heatmaps which visualize the enrichment of genomic signals
to specific target regions.

### Vignettes

There are several vignettes in the package. Each vignette focuses on a specific topic. Following
lists the general topics discussed in these vignettes:

  1. [**Making a Single Heatmap**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s2.single_heatmap.html)

    This vignette introduces the basic configuration for making a single heatmap. Similar as other
    R functions/packages, the basic usage is quite similar, but there are several unique features
    for **ComplexHeamtap** package.
    - Works both for numeric matrix and character matrix
    - For numeric matrix which contains continuous values, the package allows a color mapping function
      which can give more accurate colors and be robust to outliers.
    - Highly flexible for clustering. You can define the distance method for clustering by:
          * a pre-defined distance such as "euclidean" or "pearson"
          * a self-defined function which calculates distance from a matrix.
          * a self-defined function which calculates distance from two vectors
        
        You can define the clustering method by:
          * a clustering function such as `diana()` from **cluster** package
          * a `hclust` or `dendrogram` object.
    - `NA` is allowed for clustering and heatmap visualization.
    - Dendrogram and dimension names can be put on any side of the heatmap.
    - Rows on the heatmap can be split by `cutree`, by `kmeans` or by a data frame which contains 
      different levels that split the heatmap.
    - The heatmap body itself can be completely self-defined.

  2. [**Making a List of Heatmaps**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s3.a_list_of_heatmaps.html)

    This vignette introduces how to concatenate a list of heatmaps and how adjustment is applied to keep
    the correspondence of the heatmaps.

  3. [**Heatmap Annotations**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s4.heatmap_annotation.html)

    This vignette introduces the concept of the heatmap annotation and demonstrate how to make simple annotations
    as well as complex annotations. Also, the vignette explains the difference between column annotations
    and row annotations.

  4. [**Heatmap and Annotation Legends**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s5.legend.html)

    This vignette introduces how to configurate the heatmap legend and annotation legend, also
    how to add self-defined legends.

  5. [**Heatmap Decoration**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s6.heatmap_decoration.html)

    This vignette introduces methods to add more self-defined graphics to the heatmaps after the heatmaps
    are generated.

  6. [**Interactive with Heatmaps**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s7.interactive.html)

    How to select a region in the heatmap to retrieve the sub-matrix.
  
  7. [**OncoPrint**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s8.oncoprint.html)

    How to make an oncoPrint.

  8. [**Examples**](http://www.bioconductor.org/packages/devel/bioc/vignettes/ComplexHeatmap/inst/doc/s9.examples.html)

    More simulated and real-world examples are shown in this vignette.
    

### Visualize high dimensional genomic data

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

### OncoPrint

<a href="http://www.cbioportal.org/faq.jsp#what-are-oncoprints">OncoPrint</a> visualize multiple genomic alteration
events through a heatmap. From verion 1.3.0, **ComplexHeatmap** package provides a new `oncoPrint()` function. By this
function, users can define their own graphics which correspond to differnet alteration events. Also the function additionally
add barplots on two sides of the heatmap which tell number of different alterations in patients or samples.

With general functionality of **ComplexHeamtap**, you can add more heatmaps / row annotations to the oncoPrint, even split the
oncoPrint to enphasize sub groups.

![oncoprint](https://cloud.githubusercontent.com/assets/449218/9370313/9c9b6b00-46cf-11e5-9740-c5c2a7a40eb5.png)

### Interact with heatmaps

You can use mouse to select a region on the heatmap, it will return row index and column index which correspond to the selected region.

<p><img src="https://cloud.githubusercontent.com/assets/449218/10479344/2981c27a-7264-11e5-9868-7400c5dc620d.gif", width="600"></p>

### License

GPL (>= 2)
