library(circlize)
library(ComplexHeatmap)
library(GetoptLong)

set.seed(123)
lt = list(a = sample(letters, 10),
	      b = sample(letters, 15),
	      c = sample(letters, 20))

m = make_comb_mat(lt)
t(m)
set_name(m)
comb_name(m)
set_size(m)
comb_size(m)
lapply(comb_name(m), function(x) extract_comb(m, x))
draw(UpSet(m))
draw(UpSet(m, comb_col = c(rep(2, 3), rep(3, 3), 1)))
draw(UpSet(t(m)))

set_name(t(m))
comb_name(t(m))
set_size(t(m))
comb_size(t(m))
lapply(comb_name(t(m)), function(x) extract_comb(t(m), x))

m = make_comb_mat(lt, mode = "intersect")
lapply(comb_name(m), function(x) extract_comb(m, x))
draw(UpSet(m))

m = make_comb_mat(lt, mode = "union")
lapply(comb_name(m), function(x) extract_comb(m, x))
draw(UpSet(m))

f = system.file("extdata", "movies.csv", package = "UpSetR")
if(file.exists(f)) {
	movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), header = T, sep = ";")
	m = make_comb_mat(movies, top_n_sets = 6)
	t(m)
	set_name(m)
	comb_name(m)
	set_size(m)
	comb_size(m)
	lapply(comb_name(m), function(x) extract_comb(m, x))

	set_name(t(m))
	comb_name(t(m))
	set_size(t(m))
	comb_size(t(m))
	lapply(comb_name(t(m)), function(x) extract_comb(t(m), x))

	draw(UpSet(m))
	draw(UpSet(t(m)))

	m = make_comb_mat(movies, top_n_sets = 6, mode = "intersect")
	m = make_comb_mat(movies, top_n_sets = 6, mode = "union")
}

library(circlize)
library(GenomicRanges)
lt = lapply(1:4, function(i) generateRandomBed())
lt = lapply(lt, function(df) GRanges(seqnames = df[, 1], ranges = IRanges(df[, 2], df[, 3])))
names(lt) = letters[1:4]
m = make_comb_mat(lt)

# if(0) {
# set.seed(123)
# lt = list(a = sample(letters, 10),
# 	      b = sample(letters, 15),
# 	      c = sample(letters, 20))
# v = gplots::venn(lt, show.plot = FALSE)
# rownames(v) = apply(v[, -1], 1, paste, collapse = "")
# m = make_comb_mat(lt)
# cs = structure(comb_size(m), names = comb_name(m))
# }

if(file.exists(f)) {
	movies <- read.csv(f, header = T, sep = ";")
	genre = c("Action", "Romance", "Horror", "Children", "SciFi", "Documentary")
	rate = cut(movies$AvgRating, c(0, 1, 2, 3, 4, 5))
	m_list = tapply(seq_len(nrow(movies)), rate, function(ind) {
		make_comb_mat(movies[ind, genre, drop = FALSE])
	})
	m_list2 = normalize_comb_mat(m_list)

	lapply(m_list2, set_name)
	lapply(m_list2, set_size)
	lapply(m_list2, comb_name)
	lapply(m_list2, comb_size)

	lapply(1:length(m_list), function(i) {
		n1 = comb_name(m_list[[i]])
		x1 = comb_size(m_list[[i]])
		n2 = comb_name(m_list2[[i]])
		x2 = comb_size(m_list2[[i]])
		l = n2 %in% n1
		x2[!l]
	})
}

