

mat = matrix(rnorm(100), nrow = 10)
f = anno_boxplot(mat)
grid.newpage(); f(1:10)
f = anno_histogram(mat)
grid.newpage(); f(1:10)
f = anno_density(mat)
grid.newpage(); f(1:10)

f = anno_boxplot(mat, which = "row")
grid.newpage(); f(1:4)

lt = lapply(1:4, function(i) rnorm(8))
f = anno_boxplot(lt)
grid.newpage(); f(1:4)

