\name{normalize_comb_mat}
\alias{normalize_comb_mat}
\title{
Normalize a list of combination matrice
}
\description{
Normalize a list of combination matrice
}
\usage{
normalize_comb_mat(..., full_comb_sets = FALSE, complement_set = FALSE)
}
\arguments{

  \item{...}{Combination matrices.}
  \item{full_comb_sets}{Whether the combination matrices contain the full sets of combination sets?}
  \item{complement_set}{Whether the combination matrices also contain the complement set?}

}
\details{
It normalizes a list of combination matrice to make them have same number and order of sets and combination sets.

The sets (by \code{\link{set_name}}) from all combination matrice should be the same.
}
\examples{
# There is no example
NULL

}
