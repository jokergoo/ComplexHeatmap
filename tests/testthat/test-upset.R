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
UpSet(m)
UpSet(t(m))

set_name(t(m))
comb_name(t(m))
set_size(t(m))
comb_size(t(m))
lapply(comb_name(t(m)), function(x) extract_comb(t(m), x))

m = make_comb_mat(lt, mode = "intersect")
lapply(comb_name(m), function(x) extract_comb(m, x))
UpSet(m)

m = make_comb_mat(lt, mode = "union")
lapply(comb_name(m), function(x) extract_comb(m, x))
UpSet(m)


movies <- read.csv(system.file("extdata", "movies.csv", package = "UpSetR"), 
    header = T, sep = ";")
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

UpSet(m)
UpSet(t(m))

m = make_comb_mat(movies, top_n_sets = 6, mode = "intersect")
m = make_comb_mat(movies, top_n_sets = 6, mode = "union")



library(circlize)
library(GenomicRanges)
lt = lapply(1:4, function(i) generateRandomBed())
lt = lapply(lt, function(df) GRanges(seqnames = df[, 1], ranges = IRanges(df[, 2], df[, 3])))
names(lt) = letters[1:4]
m = make_comb_mat(lt)


