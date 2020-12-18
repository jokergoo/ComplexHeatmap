\name{set_nameAssign}
\alias{set_nameAssign}
\alias{set_name<-}
\title{
Modify Set Names
}
\description{
Modify Set Names
}
\usage{
set_name(x, ...) <- value
}
\arguments{

  \item{x}{A combination matrix returned by \code{\link{make_comb_mat}}.}
  \item{value}{New set names.}
  \item{...}{Other arguments.}

}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
set_name(m) = c("A", "B", "C")
m
}
