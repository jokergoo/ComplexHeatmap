\name{set_size}
\alias{set_size}
\title{
Set Sizes
}
\description{
Set Sizes
}
\usage{
set_size(m)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}

}
\value{
A vector of set sizes.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
set_size(m)
}
