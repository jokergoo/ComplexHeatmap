\name{comb_name}
\alias{comb_name}
\title{
Names of the Combination sets
}
\description{
Names of the Combination sets
}
\usage{
comb_name(m, readable = FALSE)
}
\arguments{

  \item{m}{A combination matrix returned by \code{\link{make_comb_mat}}.}
  \item{readable}{Whether the combination represents as e.g. "A&B&C".}

}
\details{
The name of the combination sets are formatted as a string
of binary bits. E.g. for three sets of "a", "b", "c", the combination
set with name "101" corresponds to select set a, not select set b
and select set c. The definition of "select" depends on the value of
\code{mode} from \code{\link{make_comb_mat}}.
}
\value{
A vector of names of the combination sets.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
          b = sample(letters, 15),
          c = sample(letters, 20))
m = make_comb_mat(lt)
comb_name(m)
comb_name(m, readable = TRUE)
}
