\name{comb_size}
\alias{comb_size}
\title{
Sizes of the Combination sets
}
\description{
Sizes of the Combination sets
}
\usage{
comb_size(m, degree = NULL)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}
  \item{degree}{degree of the intersection. The value can be a vector.}

}
\value{
A vector of sizes of the combination sets.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
comb_size(m)
}
