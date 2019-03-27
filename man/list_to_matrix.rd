\name{list_to_matrix}
\alias{list_to_matrix}
\title{
Convert a List of Sets to a Binary Matrix
}
\description{
Convert a List of Sets to a Binary Matrix
}
\usage{
list_to_matrix(lt, universal_set = NULL)
}
\arguments{

  \item{lt}{A list of vectors.}
  \item{universal_set}{The universal set.}

}
\details{
It converts the list which have m sets to a binary matrix with n rows and m columns
where n is the size of universal set.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 5),
          b = sample(letters, 10),
          c = sample(letters, 15))
list_to_matrix(lt)
list_to_matrix(lt, universal_set = letters)
}
