\name{extract_comb}
\alias{extract_comb}
\title{
Extract Elements in a Combination set
}
\description{
Extract Elements in a Combination set
}
\usage{
extract_comb(m, comb_name)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}
  \item{comb_name}{The valid combination set name should be from \code{\link{comb_name}}.}

}
\details{
It returns the combination set.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
extract_comb(m, "110")
}
