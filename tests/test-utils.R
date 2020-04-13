library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

# things needed to be tested
# 1. the order
# 2. if the sum of sizes are larger than xlim

make_plot = function(pos1, pos2, range) {
	oxpd = par("xpd")
	par(xpd = NA)
	plot(NULL, xlim = c(0, 4), ylim = range, ann = FALSE)
	col = rand_color(nrow(pos1), transparency = 0.5)
	rect(0.5, pos1[, 1], 1.5, pos1[, 2], col = col)
	rect(2.5, pos2[, 1], 3.5, pos2[, 2], col = col)
	segments(1.5, rowMeans(pos1), 2.5, rowMeans(pos2))
	par(xpd = oxpd)
}

range = c(0, 10)
pos1 = rbind(c(1, 2), c(5, 7))
make_plot(pos1, smartAlign2(pos1, range = range), range)

range = c(0, 10)
pos1 = rbind(c(-0.5, 2), c(5, 7))
make_plot(pos1, smartAlign2(pos1, range = range), range)

pos1 = rbind(c(-1, 2), c(3, 4), c(5, 6), c(7, 11))
pos1 = pos1 + runif(length(pos1), max = 0.3, min = -0.3)
par(mfrow = c(3, 3))
for(i in 1:9) {
	ind = sample(4, 4)
	make_plot(pos1[ind, ], smartAlign2(pos1[ind, ], range = range), range)
}
par(mfrow = c(1, 1))

pos1 = rbind(c(3, 6), c(4, 7))
make_plot(pos1, smartAlign2(pos1, range = range), range)

pos1 = rbind(c(1, 8), c(3, 10))
make_plot(pos1, smartAlign2(pos1, range = range), range)

########## new version of smartAlign2() ############

start = c(0.0400972528391016, 0.0491583597430212, 0.0424302664385027, 0.0547524243812509, 0.0820937279769642, 0.126861283282835, 0.178503822565168, 0.327742831447437, 0.570671411156898, 0.81775868755151)
end = c(0.0921142856224367, 0.107091640256979, 0.137858195099959, 0.159189883311057, 0.177521656638421, 0.20727333210178, 0.304669254357909, 0.463122553167947, 0.676924742689255, 0.929837466294643)
range = c(0, 1)
smartAlign2(start, end, range, plot = TRUE)


start <- c(0.722121284290678, 0.701851666769472, 0.284795592003117, 0.335674695572052, 0.246977082249377, 0.767289857630785, 0.728198060058033, 0.299241440370817, -0.0149946764559372, 0.85294351791166, 0.126216621670218, 0.478169948493225)
end <- c(0.766196472718668, 0.763101604258565, 0.34604552949221, 0.421334650222341, 0.344144413077725, 0.847196123677626, 0.813858014708322, 0.392347344675911, 0.108452620381171, 0.969486388630396, 0.249951602628847, 0.584914163656308)
od = order(start)
start = start[od]; end = end[od]
range = c(0, 1)
pos = smartAlign2(start, end, range)
n = nrow(pos)
pos[1:(n-1), 2] > pos[2:n, 1]


if(0) {
	go_id = random_GO(500)
	mat = GO_similarity(go_id)
	invisible(simplify(mat, order_by_size = FALSE))
}
