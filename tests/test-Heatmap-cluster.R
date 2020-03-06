library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

# ht_opt("verbose" = TRUE)
m = matrix(rnorm(50), nr = 10)

ht = Heatmap(m)
ht = make_row_cluster(ht)

ht = Heatmap(m, cluster_rows = FALSE)
ht = make_row_cluster(ht)

ht = Heatmap(m, row_km = 2)
ht = make_row_cluster(ht)

ht = Heatmap(m, row_split = sample(letters[1:2], 10, replace = TRUE))
ht = make_row_cluster(ht)

ht = Heatmap(m, cluster_rows = hclust(dist(m)))
ht = make_row_cluster(ht)

ht = Heatmap(m, cluster_rows = hclust(dist(m)), row_split = 2)
ht = make_row_cluster(ht)

# ht_opt("verbose" = FALSE)
