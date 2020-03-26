\name{make_comb_mat}
\alias{make_comb_mat}
\title{
Make a Combination Matrix for UpSet Plot
}
\description{
Make a Combination Matrix for UpSet Plot
}
\usage{
make_comb_mat(..., mode = c("distinct", "intersect", "union"),
    top_n_sets = Inf, min_set_size = -Inf,
    universal_set = NULL, complement_size = NULL,
    value_fun = NULL, set_on_rows = TRUE)
}
\arguments{

  \item{...}{The input sets. If it is represented as a single variable, it should be a matrix/data frame or a list. If it is multiple variables, it should be name-value pairs, see Input section for explanation.}
  \item{mode}{The mode for forming the combination set, see Mode section.}
  \item{top_n_sets}{Number of sets with largest size.}
  \item{min_set_size}{Ths minimal set size that is used for generating the combination matrix.}
  \item{universal_set}{The universal set. If it is set, the size of the complement set of all sets is also calculated. It if is specified, \code{complement_size} is ignored.}
  \item{complement_size}{The size for the complement of all sets. If it is specified, the combination set name will be like "00...".}
  \item{value_fun}{For each combination set, how to calculate the size? If it is a scalar set,  the length of the vector is the size of the set, while if it is a region-based set, (i.e. \code{GRanges} or \code{IRanges} object), the sum of widths of regions in the set is calculated as the size of the set.}
  \item{set_on_rows}{Used internally.}

}
\section{Input}{
To represent multiple sets, the variable can be represented as:

1. A list of sets where each set is a vector, e.g.:

  \preformatted{
    list(set1 = c("a", "b", "c"),
         set2 = c("b", "c", "d", "e"),
         ...)  }

2. A binary matrix/data frame where rows are elements and columns are sets, e.g.:

  \preformatted{
      a b c
    h 1 1 1
    t 1 0 1
    j 1 0 0
    u 1 0 1
    w 1 0 0
    ...  }

If the variable is a data frame, the binary columns (only contain 0 and 1) and the logical
columns are only kept.

The set can be genomic regions, then it can only be represented as a list of \code{GRanges} objects.}
\section{Mode}{
E.g. for three sets (A, B, C), the UpSet approach splits the combination of selecting elements
in the set or not in the set and calculates the sizes of the combination sets. For three sets,
all possible combinations are:

  \preformatted{
    A B C
    1 1 1
    1 1 0
    1 0 1
    0 1 1
    1 0 0
    0 1 0
    0 0 1  }

A value of 1 means to select that set and 0 means not to select that set. E.g., "1 1 0"
means to select set A, B while not set C. Note there is no "0 0 0", because the background 
size is not of interest here. With the code of selecting and not selecting the sets, next
we need to define how to calculate the size of that combination set. There are three modes:

1. \code{distinct} mode: 1 means in that set and 0 means not in that set, then "1 1 0" means a
set of elements also in set A and B, while not in C (i.e. \code{setdiff(intersect(A, B), C)}). Under
this mode, the seven combination sets are the seven partitions in the Venn diagram and they
are mutually exclusive.

2. \code{intersect} mode: 1 means in that set and 0 is not taken into account, then, "1 1 0" means
a set of elements in set A and B, and they can also in C or not in C (i.e. \code{intersect(A, B)}).
Under this mode, the seven combination sets can overlap.

3. \code{union} mode: 1 means in that set and 0 is not taken into account. When there are multiple
1, the relationship is OR. Then, "1 1 0" means a set of elements in set A or B, and they can also in C or not in C (i.e. \code{union(A, B)}).
Under this mode, the seven combination sets can overlap.}
\value{
A matrix also in a class of \code{comb_mat}.

Following functions can be applied to it: \code{\link{set_name}}, \code{\link{comb_name}}, \code{\link{set_size}}, \code{\link{comb_size}}, \code{\link{comb_degree}},
\code{\link{extract_comb}} and \code{\link{t.comb_mat}}.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)

mat = list_to_matrix(lt)
mat
m = make_comb_mat(mat)

\dontrun{
require(circlize)
require(GenomicRanges)
lt = lapply(1:4, function(i) generateRandomBed())
lt = lapply(lt, function(df) GRanges(seqnames = df[, 1], 
    ranges = IRanges(df[, 2], df[, 3])))
names(lt) = letters[1:4]
m = make_comb_mat(lt)
}
}
