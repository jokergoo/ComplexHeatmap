\name{[.comb_mat}
\alias{Extract.comb_mat}
\alias{[.comb_mat}
\title{
Subset the Combination Matrix
}
\description{
Subset the Combination Matrix
}
\usage{
\method{[}{comb_mat}(x, i, j, drop = FALSE)
}
\arguments{

  \item{x}{A combination matrix returned by \code{\link{make_comb_mat}}.}
  \item{i}{Indices on rows.}
  \item{j}{Indices on columns.}
  \item{drop}{It is always reset to \code{FALSE} internally.}

}
\details{
If sets are on rows of the combination matrix, the row indices correspond
to sets and column indices correspond to combination sets, and if sets are
on columns of the combination matrix, rows correspond to the combination sets.

If the index is one-dimension, e.g. \code{x[i]}, the index always corresponds to the combination sets.

You should not subset by the sets. It will give you wrong combination set size. The subsetting
on sets are only used internally.

This subsetting method is mainly for subsetting combination sets, i.e., users
can first use \code{\link{comb_size}} to get the size of each combination set, and filter them
by the size.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
m2 = m[, comb_size(m) >= 3]
comb_size(m2)
m[comb_size(m) >= 3]
}
