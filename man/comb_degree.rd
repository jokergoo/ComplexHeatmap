\name{comb_degree}
\alias{comb_degree}
\title{
Degrees of the Combination sets
}
\description{
Degrees of the Combination sets
}
\usage{
comb_degree(m)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}

}
\details{
The degree for a combination set is the number of sets that are selected.
}
\value{
A vector of degrees of the combination sets.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
comb_degree(m)
}
