Make Complex Heatmap
=========================

The aim of this package is to place any numbers of heatmaps, and any type of annotation graphics (both on rows and columns).

The package makes heatmaps in an object oriented way by abstracting heatmaps into several classes:

- `Heatmap` a single heatmap
- `HeatmapList` a list of heatmaps
- `HeatmapAnnotation` annotation on columns or rows

## Usage

Make a single heatmap:

```{r}
Heatmap(...)
```

A single Heatmap with column annotations:

```{r}
Heatmap(..., top_annotation = HeatmapAnnotation(...))
```

Make a list of heatmaps:

```{r}
Heatmap(...) + Heatmap(...)
```

Make a list of heatmaps and row annotations:

```{r}
Heatmap(...) + HeatmapAnnotation(..., which = "row")
```

## Examples

![default](https://cloud.githubusercontent.com/assets/449218/6541828/75c77f8a-c4e5-11e4-80af-6ebb5e649898.png)
![1](https://cloud.githubusercontent.com/assets/449218/6541827/75b8c620-c4e5-11e4-954e-1c0709b9f1b7.png)
![example](https://cloud.githubusercontent.com/assets/449218/6862097/1bc46436-d443-11e4-91f5-431bc9210c80.png)
