\name{set_name}
\alias{set_name}
\title{
Set Names
}
\description{
Set Names
}
\usage{
set_name(m)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}

}
\value{
A vector of set names.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
set_name(m)
}
