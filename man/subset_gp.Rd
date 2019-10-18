\name{subset_gp}
\alias{subset_gp}
\title{
Subset a gpar Object
}
\description{
Subset a gpar Object
}
\usage{
subset_gp(gp, i)
}
\arguments{

  \item{gp}{A \code{\link{gpar}} object.}
  \item{i}{A vector of indices.}

}
\value{
A \code{\link[grid]{gpar}} object.
}
\examples{
gp = gpar(col = 1:10, fill = 1)
subset_gp(gp, 1:5)
}
