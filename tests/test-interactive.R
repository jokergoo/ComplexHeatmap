
if(0) {

m = matrix(rnorm(100), 10)
rownames(m) = 1:10
colnames(m) = 1:10

ht = Heatmap(m)
ht = draw(ht)
selectArea(ht)



ht = Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
selectArea(ht)


ht = Heatmap(m, row_km = 2, column_km = 2) + Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
selectArea(ht)

pdf("~/test.pdf")
ht = Heatmap(m)
ht = draw(ht)
selectArea(ht, pos1 = unit(c(1, 1), "cm"), pos2 = unit(c(4, 4), "cm"), verbose = TRUE)

set.seed(123)
ht = Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
selectArea(ht, pos1 = unit(c(1, 1), "cm"), pos2 = unit(c(8, 8), "cm"), verbose = TRUE)
dev.off()

png("~/test-1.png")
ht = Heatmap(m)
ht = draw(ht)
selectArea(ht, pos1 = unit(c(1, 1), "cm"), pos2 = unit(c(4, 4), "cm"), verbose = TRUE)
dev.off()

png("~/test-2.png")
set.seed(123)
ht = Heatmap(m, row_km = 2, column_km = 2)
ht = draw(ht)
selectArea(ht, pos1 = unit(c(1, 1), "cm"), pos2 = unit(c(8, 8), "cm"), verbose = TRUE)
dev.off()

}