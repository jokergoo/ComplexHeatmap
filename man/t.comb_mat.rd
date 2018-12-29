\name{t.comb_mat}
\alias{t.comb_mat}
\title{
Transpost the Combination Matrix
}
\description{
Transpost the Combination Matrix
}
\usage{
\method{t}{comb_mat}(x)
}
\arguments{

  \item{x}{A combination matrix returned by \code{\link{make_comb_mat}}.}

}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
t(m)
}
