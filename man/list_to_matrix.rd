\name{list_to_matrix}
\alias{list_to_matrix}
\title{
Convert a List of Sets to a Binary Matrix
}
\description{
Convert a List of Sets to a Binary Matrix
}
\usage{
list_to_matrix(lt)
}
\arguments{

  \item{lt}{A list of vectors.}

}
\details{
It converts the list which have m sets to a binary matrix with n rows and m columns
where n is the number of union of all sets in the list.
}
\examples{
set.seed(123)
lt = list(a = sample(letters, 10),
	      b = sample(letters, 15),
	      c = sample(letters, 20))
list_to_matrix(lt)
}
